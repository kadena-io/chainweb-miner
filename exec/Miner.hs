{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module: Main
-- Copyright: Copyright © 2019 Kadena LLC.
-- License: MIT
-- Maintainer: Colin Woodbury <colin@kadena.io>
-- Stability: experimental
--
-- The fast "inner loop" of the mining process. Asks for work from a Chainweb
-- Node, and submits results back to it.
--
-- == Purpose and Expectations ==
--
-- This tool is a low-level, pull-based, independent, focusable, multicore CPU
-- and GPU miner for Chainweb. By this we mean:
--
--   * low-level: The miner is not aware of how `BlockHeader`s are encoded into
--     `ByteString`s, as indicated by an external spec. It does not know how to
--     construct a full `BlockHeader` type, nor does it need to. It has no
--     concept of `Cut`s or the cut network used by Chainweb - it simply
--     attempts `Nonce`s until a suitable hash is found that matches the current
--     `HashTarget`.
--
--   * pull-based: Work is requested from some remote Chainweb Node, as
--     configured on the command line. The Miner is given an encoded candidate
--     `BlockHeader`, which it then injects a `Nonce` into, hashes, and checks
--     for a solution. If/when a solution is found, it is sent back to the
--     Chainweb Node to be reassociated with its Payload and published as a new
--     `Cut` to the network.
--
--   * independent: It is assumed that in general, individuals running Chainweb
--     Miners and Chainweb Nodes are separate entities. A Miner requests work
--     from a Node and trusts them to assemble a block. Nodes do not know who is
--     requesting work, but Miners know who they're requesting work from. In
--     this way, there is a many-to-one relationship between Mining Clients and
--     a Node.
--
--   * focusable: A Miner can be configured to prioritize work belonging to a
--     specific chain. Note, however, that if a work request for a particular
--     chain can't be filled by a Node (if say that Chain has progressed too far
--     relative to its neighbours), then the Node will send back whatever it
--     could find. This strategy is to balance the needs of Miners who have a
--     special interest in progressing a specific chain with the needs of the
--     network which requires even progress across all chains.
--
--   * multicore: The miner uses 1 CPU core by default, but can use as many as
--     you indicate. GPU support is also available.
--

module Main ( main ) where

import           Control.Retry
import           Control.Scheduler hiding (traverse_)
import           Data.Bytes.Get
import           Data.Bytes.Put
import qualified Data.ByteString.Short as BS
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Tuple.Strict (T2(..), T3(..))
import           Network.HTTP.Client hiding (Proxy(..), responseBody)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Types.Status (Status(..))
import           Options.Applicative
import           RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import           RIO.List.Partial (head)
import qualified RIO.NonEmpty as NEL
import qualified RIO.NonEmpty.Partial as NEL
import qualified RIO.Text as T
import           Servant.Client
import qualified System.Path as Path
import qualified System.Random.MWC as MWC
import           Text.Printf (printf)

-- internal modules

import           Chainweb.BlockHeader
import           Chainweb.BlockHeader.Validation (prop_block_pow)
import           Chainweb.BlockHeight
import           Chainweb.Cut.Create
import           Chainweb.Difficulty
import           Chainweb.Miner.Core
import           Chainweb.Miner.RestAPI.Client (solvedClient, workClient)
import           Chainweb.RestAPI.NodeInfo (NodeInfo(..), NodeInfoApi)
import           Chainweb.Utils (runGet)
import           Chainweb.Version
import           Miner.Balance
import           Miner.Types
import           Miner.Updates
import qualified Pact.Types.Crypto as P hiding (PublicKey)
import qualified Pact.Types.Util as P

--------------------------------------------------------------------------------
-- Work

main :: IO ()
main = execParser opts >>= \case
    cmd@(CPU _ cargs) -> work cmd cargs >> exitFailure
    cmd@(GPU _ cargs) -> work cmd cargs >> exitFailure
    Otherwise Keys -> genKeys
    Otherwise (Balance url account) -> getBalances url account
  where
    opts :: ParserInfo Command
    opts = info (pCommand <**> helper)
        (fullDesc <> progDesc "The Official Chainweb Mining Client")

work :: Command -> ClientArgs -> IO ()
work cmd cargs = do
    lopts <- setLogMinLevel (ll cargs) . setLogUseLoc False <$> logOptionsHandle stderr True
    withLogFunc lopts $ \logFunc -> do
        g <- MWC.createSystemRandom
        m <- newManager (mkManagerSettings tlsSettings Nothing)
        euvs <- sequence <$> traverse (nodeVer m) (coordinators cargs)
        case euvs of
            Left e -> throwString $ show e
            Right results -> do
                mUrls <- newIORef $ NEL.fromList results
                stats <- newIORef 0
                start <- newIORef 0
                successStart <- getPOSIXTime >>= newIORef
                updateMap <- newUpdateMap
                runRIO (Env g m logFunc cmd cargs stats start successStart updateMap mUrls) run
  where
    nodeVer :: Manager -> BaseUrl -> IO (Either ClientError (T2 BaseUrl ChainwebVersion))
    nodeVer m baseurl = (T2 baseurl <$>) <$> getInfo m baseurl



getInfo :: Manager -> BaseUrl -> IO (Either ClientError ChainwebVersion)
getInfo m url = fmap nodeVersion <$> runClientM (client (Proxy @NodeInfoApi)) cenv
  where
    cenv = mkClientEnv m url

run :: RIO Env ()
run = do
    env <- ask
    logInfo "Starting Miner."
    mining (scheme env)

scheme :: Env -> (TargetBytes -> HeaderBytes -> RIO Env HeaderBytes)
scheme env = case envCmd env of
    CPU e _ -> cpu e
    GPU e _ -> gpu e
    _       -> error "Impossible: You shouldn't reach this case."

genKeys :: IO ()
genKeys = do
    kp <- P.genKeyPair P.defaultScheme
    printf "public:  %s\n" (T.unpack . P.toB16Text $ P.getPublic kp)
    printf "private: %s\n" (T.unpack . P.toB16Text $ P.getPrivate kp)

newtype GetWorkFailure = GetWorkFailure SomeException
    deriving (Show, Display)

instance Exception GetWorkFailure

-- | Attempt to get new work while obeying a sane retry policy.
--
getWork :: RIO Env (T2 WorkBytes UpdateKey)
getWork = do
    logDebug "Attempting to fetch new work from the remote Node"
    e <- ask
    retrying policy (const warn) (const . liftIO $ f e) >>= \case
        Left err -> throwM $ GetWorkFailure $ toException err
        Right bs -> T2 bs <$> workKey bs
  where
    -- | If we wait longer than the average block time and still can't get
    -- anything, then there's no point in continuing to wait.
    --
    policy :: RetryPolicy
    policy = exponentialBackoff 500000 <> limitRetries 7

    warn :: Either ClientError WorkBytes -> RIO Env Bool
    warn (Right _) = pure False
    warn (Left se) = bad se $> True

    bad :: ClientError -> RIO Env ()
    bad (ConnectionError _) = logWarn "Could not connect to the Node."
    bad (FailureResponse _ r) = logWarn $ c <> " from Node: " <> m
      where
        c = display . statusCode $ responseStatusCode r
        m = displayBytesUtf8 . BL.toStrict $ responseBody r
    bad _ = logError "Something truly bad has happened."

    f :: Env -> IO (Either ClientError WorkBytes)
    f e = do
        T2 u v <- NEL.head <$> readIORef (envUrls e)
        runClientM (workClient v (chainid a) $ miner a) (mkClientEnv m u)
      where
        a = envArgs e
        m = envMgr e

workKey :: WorkBytes -> RIO Env UpdateKey
workKey wb = do
    T3 cbytes _ _ <- runGet unWorkBytes $ _workBytes wb
    cid <- liftIO $ chain cbytes
    return $! UpdateKey { _updateKeyChainId = cid }

-- -------------------------------------------------------------------------- --
-- Mining

-- | A supervisor thread that listens for new work and manages mining threads.
--
-- TODO: use exponential backoff instead of fixed delay when retrying.
-- (restart retry sequence when the maximum retry count it reached)
--
mining :: (TargetBytes -> HeaderBytes -> RIO Env HeaderBytes) -> RIO Env ()
mining go = miningLoop go `finally` logInfo "Miner halted."

data Recovery = Irrecoverable | Recoverable | NodeSwitch

-- | The inner mining loop.
--
-- It uses 'getWork' to obtain new work and starts mining. If a block is solved it
-- calls 'miningSuccess' and loops around.
--
-- It also starts over with new work when an update is triggered.
--
-- NOTE: if this fails continuously, e.g. because of a miss-configured or buggy
-- miner, this function will spin forever!
--
-- TODO: add rate limiting for failures and raise an error if the function fails
-- at an higher rate.
--
miningLoop :: (TargetBytes -> HeaderBytes -> RIO Env HeaderBytes) -> RIO Env ()
miningLoop inner = mask go
  where
    go :: (RIO Env () -> RIO Env a) -> RIO Env ()
    go umask = (forever (umask loopBody) `catches` handlers) >>= \case
        Irrecoverable -> pure ()
        Recoverable -> do
            threadDelay 1_000_000
            go umask
        NodeSwitch -> do
            threadDelay 1_000_000
            switchWork >>= \x -> when x (go umask)
      where
        handlers =
            [ Handler $ \(e :: IOException) -> do
                logError (display e)
                pure Irrecoverable
            , Handler $ \(e :: UpdateFailure) -> do
                logWarn "Update stream failed. Switching nodes ..."
                logDebug $ display e
                pure NodeSwitch
            , Handler $ \(e :: GetWorkFailure) -> do
                logWarn "Failed to fetch work! Switching nodes..."
                logDebug $ display e
                pure NodeSwitch
            , Handler $ \(e :: SomeException) -> do
                logWarn "Some general error in mining loop. Trying again..."
                logDebug $ display e
                pure Recoverable
            ]

    switchWork = do
        e <- ask
        urls <- readIORef $ envUrls e
        case NEL.nonEmpty $ NEL.tail urls of
            Nothing -> do
                logError "No nodes left!"
                return False
            Just rest -> do
                clearUpdateMap =<< asks envUpdateMap
                writeIORef (envUrls e) rest
                return True

    loopBody :: RIO Env ()
    loopBody = do
        T2 w key <- getWork
        T3 cbytes tbytes hbytes <- runGet unWorkBytes $ _workBytes w
        cid <- liftIO $ chainInt cbytes
        logDebug . display . T.pack $ printf "Chain %d: Start mining on new work item." cid
        withPreemption key (inner tbytes hbytes) >>= \case
            Right a -> miningSuccess w a
            Left () -> logDebug "Mining loop was preempted. Getting updated work ..."
      where
        -- | If the `go` call won the `race`, this function yields the result back
        -- to some "mining coordinator" (likely a chainweb-node).
        --
        miningSuccess :: WorkBytes -> HeaderBytes -> RIO Env ()
        miningSuccess w h = do
            e <- ask
            secs <- readIORef (envSecs e)
            hashes <- readIORef (envHashes e)
            before <- readIORef (envLastSuccess e)
            now <- liftIO getPOSIXTime
            writeIORef (envLastSuccess e) now
            let !m = envMgr e
                !r = (fromIntegral hashes :: Double) / max 1 (fromIntegral secs) / 1000000
                !d = ceiling (now - before) :: Int
            T3 cbytes _ hbytes <- runGet unWorkBytes $ _workBytes w
            cid <- liftIO $ chainInt cbytes
            hgh <- liftIO $ height hbytes
            logInfo . display . T.pack $
                printf "Chain %d: Mined block at Height %d. (%.2f MH/s - %ds since last)" cid hgh r d
            T2 url v <- NEL.head <$> readIORef (envUrls e)
            res <- liftIO . runClientM (solvedClient v h) $ mkClientEnv m url
            when (isLeft res) $ logWarn "Failed to submit new BlockHeader!"
          where

cpu :: CPUEnv -> TargetBytes -> HeaderBytes -> RIO Env HeaderBytes
cpu cpue tbytes hbytes = do
    !start <- liftIO getPOSIXTime
    e <- ask
    wh <- getWorkHeader
    T2 _ v <- NEL.head <$> readIORef (envUrls e)
    T2 new ns <- liftIO . fmap head . withScheduler comp $ \sch ->
        replicateWork (fromIntegral $ cores cpue) sch $ do
            -- TODO Be more clever about the Nonce that's picked to ensure that
            -- there won't be any overlap?
            n <- Nonce <$> MWC.uniform (envGen e)
            new <- usePowHash v $ \(_ :: Proxy a) -> mine @a n wh
            terminateWith sch $ getSolved new
    !end <- liftIO getPOSIXTime
    modifyIORef' (envHashes e) $ \hashes -> ns * fromIntegral (cores cpue) + hashes
    modifyIORef' (envSecs e) (\secs -> secs + ceiling (end - start))
    pure new
  where
    comp :: Comp
    comp = case cores cpue of
             1 -> Seq
             n -> ParN n

    getWorkHeader = runGet unsafeDecodeWorkHeader
        $ "\0\0" <> (_targetBytes tbytes) <> (_headerBytes hbytes)

    getSolved (T2 a b) = T2 (HeaderBytes $ runPutS $ encodeSolvedWork a) b

gpu :: GPUEnv -> TargetBytes -> HeaderBytes -> RIO Env HeaderBytes
gpu ge@(GPUEnv mpath margs) t@(TargetBytes target) h@(HeaderBytes blockbytes) = do
    minerPath <- liftIO . Path.makeAbsolute . Path.fromFilePath $ T.unpack mpath
    e <- ask
    res <- liftIO $ callExternalMiner minerPath (map T.unpack margs) False target blockbytes
    case res of
      Left err -> do
          logError . display . T.pack $ "Error running GPU miner: " <> err
          throwString err
      Right (MiningResult nonceBytes numNonces hps _) -> do
          let secs = numNonces `div` max 1 hps
          let r = checkNonce (nonceBytes <> B.drop 8 blockbytes)
                <|> checkNonce (B.take (B.length blockbytes - 8) blockbytes <> nonceBytes)
          case r of
            Nothing -> do
              logError "Bad nonce returned from GPU!"
              gpu ge t h
            Just newBytes -> do
              modifyIORef' (envHashes e) (+ numNonces)
              modifyIORef' (envSecs e) (+ secs)
              pure $! HeaderBytes newBytes
  where
    checkNonce newBytes = do
        bh <- runGet decodeBlockHeaderWithoutHash newBytes
        guard (prop_block_pow bh)
        return newBytes

-- -------------------------------------------------------------------------- --
-- Utils

unWorkBytes :: MonadGet m => m (T3 ChainBytes TargetBytes HeaderBytes)
unWorkBytes = T3
    <$> (ChainBytes <$> getBytes 4)
    <*> (TargetBytes <$> getBytes 32)
    <*> do
        l <- fromIntegral <$> remaining
        HeaderBytes <$> getByteString l

unsafeDecodeWorkHeader :: MonadGet m => m WorkHeader
unsafeDecodeWorkHeader = WorkHeader
    <$> decodeChainId
    <*> decodeHashTarget
    <*> do
        l <- fromIntegral <$> remaining
        BS.toShort <$> getByteString l

chain :: MonadThrow m => MonadIO m => ChainBytes -> m ChainId
chain (ChainBytes cbs) = runGet decodeChainId cbs

chainInt :: MonadThrow m => MonadIO m => ChainBytes -> m Int
chainInt c = chainIdInt <$> chain c

height :: MonadThrow m => MonadIO m => HeaderBytes -> m Word64
height (HeaderBytes hbs) = _height <$> runGet decodeBlockHeight (B.take 8 $ B.drop 258 hbs)

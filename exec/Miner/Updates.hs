{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}

module Miner.Updates
  ( newUpdateMap
  , withPreemption
  , clearUpdateMap
  , UpdateFailure(..)
  ) where

import           Data.Tuple.Strict (T2(..))

import           Network.HTTP.Client hiding (Proxy(..), responseBody)
import qualified Network.HTTP.Client as HTTP
import           Network.Wai.EventSource (ServerEvent(..))
import           Network.Wai.EventSource.Streaming (withEvents)

import           RIO
import qualified RIO.HashMap as HM
import qualified RIO.NonEmpty as NEL
import qualified RIO.Text as T

import           Servant.Client

import qualified Streaming.Prelude as SP

-- internal modules

import           Chainweb.Utils (runPut, toText)
import           Chainweb.Version (ChainId, ChainwebVersion, encodeChainId)
import           Miner.Types (Env(..), UpdateKey(..), UpdateMap(..))

-- -------------------------------------------------------------------------- --
-- Internal Trigger Type

data Reason = Timeout | Update | StreamClosed | StreamFailed SomeException
    deriving (Show)

newtype Trigger = Trigger (STM Reason)

awaitTrigger :: MonadIO m => Trigger -> m Reason
awaitTrigger (Trigger t) = atomically t

-- -------------------------------------------------------------------------- --
-- Update Map API

newtype UpdateFailure = UpdateFailure T.Text
    deriving (Show, Eq, Ord, Display)

instance Exception UpdateFailure

-- | Creates a map that maintains one upstream for each chain
--
newUpdateMap :: IO UpdateMap
newUpdateMap = UpdateMap <$> newMVar mempty

-- | Reset all update streams in the map.
--
clearUpdateMap :: MonadUnliftIO m => UpdateMap -> m ()
clearUpdateMap (UpdateMap um) = modifyMVar um $ \m -> do
    mapM_ (\(T2 _ a) -> cancel a) m
    return mempty

getTrigger :: UpdateMap -> UpdateKey -> RIO Env Trigger
getTrigger (UpdateMap v) k = modifyMVar v $ \m -> case HM.lookup k m of

    -- If there exists already an update stream, check that it's live, and
    -- restart if necessary.
    --
    Just x -> do
        n@(T2 var a) <- checkStream x
        t <- newTrigger var a
        return (HM.insert k n m, t)

    -- If there isn't an update stream in the map, create a new one.
    --
    Nothing -> do
        n@(T2 var a) <- newTVarIO 0 >>= newUpdateStream
        t <- newTrigger var a
        return (HM.insert k n m, t)
  where
    checkStream :: T2 (TVar Int) (Async ()) -> RIO Env (T2 (TVar Int) (Async ()))
    checkStream (T2 var a) = poll a >>= \case
        Nothing -> return (T2 var a)
        Just (Left _) -> newUpdateStream var -- TODO logging, throttling
        Just (Right _) -> newUpdateStream var

    newUpdateStream :: TVar Int -> RIO Env (T2 (TVar Int) (Async ()))
    newUpdateStream var = T2 var
        <$!> async (updateStream (_updateKeyChainId k) var)

    -- There are three possible outcomes
    --
    newTrigger :: TVar Int -> Async () -> RIO Env Trigger
    newTrigger var a = do
        cur <- readTVarIO var
        timeoutVar <- registerDelay (5 * 30_000_000)
            -- 5 times the block time ~ 0.7% of all blocks. This for detecting if
            -- a stream gets stale without failing.

        return $ Trigger $ pollSTM a >>= \case
            Just (Right ()) -> return StreamClosed
            Just (Left e) -> return $ StreamFailed e
            Nothing -> do
                isTimeout <- readTVar timeoutVar
                isUpdate <- (/= cur) <$> readTVar var
                unless (isTimeout || isUpdate) retrySTM
                return Update

-- | Run an operation that is preempted if an update event occurs.
--
-- Streams are restarted automatically, when they got closed by the server. We
-- don't restart streams automatically in case of a failure, but instead throw
-- an exception. Failures are supposed to be handled in the outer mining
-- functions.
--
-- There is risk that a stream stalls without explicitely failing. We solve this
-- by preempting the loop if we haven't seen an update after 5 times the block
-- time (which will affect about 0.7% of all blocks).
--
withPreemption :: UpdateKey -> RIO Env a -> RIO Env (Either () a)
withPreemption k = race awaitChange
  where
    awaitChange = do
        m <- asks envUpdateMap
        trigger <- getTrigger m k
        awaitTrigger trigger >>= \case
            StreamClosed -> awaitChange
            StreamFailed e -> throwM $ UpdateFailure $ "update stream failed: " <> errMsg e
            Timeout -> throwM $ UpdateFailure "timeout of update stream"
            Update -> return ()

    errMsg e = case fromException e of
        Just (HTTP.HttpExceptionRequest _ ex) -> T.pack $ show ex
        _                                     -> T.pack $ show e

-- | Atomatically restarts the stream when the response status is 2** and throws
-- and exception otherwise.
--
updateStream :: ChainId -> TVar Int -> RIO Env ()
updateStream cid var = do
    e <- ask
    u <- NEL.head <$> readIORef (envUrls e) -- Do we ever use something else than the head?
    liftIO $ withEvents (req u) (envMgr e) $ \updates -> updates
        & SP.filter realEvent
        & SP.mapM_ (\_ -> atomically $ modifyTVar' var (+ 1))
  where
    realEvent :: ServerEvent -> Bool
    realEvent ServerEvent{} = True
    realEvent _             = False

    req :: T2 BaseUrl ChainwebVersion -> Request
    req (T2 u v) = defaultRequest
        { host = encodeUtf8 . T.pack . baseUrlHost $ u
        , path = "chainweb/0.0/" <> encodeUtf8 (toText v) <> "/mining/updates"
        , port = baseUrlPort u
        , secure = True
        , method = "GET"
        , requestBody = RequestBodyBS $ runPut (encodeChainId cid)
        , responseTimeout = responseTimeoutNone
        , checkResponse = throwErrorStatusCodes
        }

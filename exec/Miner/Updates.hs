{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Miner.Updates
  ( UpdateKey(..)
  , UpdateMap(..)
  , newUpdateMap
  , withPreemption
  ) where

import           Data.Tuple.Strict (T2(..))
import           Network.HTTP.Client hiding (Proxy(..), responseBody)
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
import           Miner.Types (Env(..))

---

-- Maintains one upstream for each url and chain
--
-- TODO;
--
-- * implement reaper thread
-- * implement shutdown that closes all connections
--

data UpdateKey = UpdateKey
    { _updateKeyHost    :: !String
    , _updateKeyPort    :: !Int
    , _updateKeyChainId :: !ChainId }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable)

newtype UpdateMap = UpdateMap
    { _updateMap :: MVar (HM.HashMap UpdateKey (T2 (TVar Int) (Async ())))
    }

newUpdateMap :: MonadIO m => m UpdateMap
newUpdateMap = UpdateMap <$> newMVar mempty

getUpdateVar :: UpdateMap -> UpdateKey -> RIO Env (TVar Int)
getUpdateVar (UpdateMap v) k = modifyMVar v $ \m -> case HM.lookup k m of
    Just x -> do
        n@(T2 var _) <- checkStream x
        return (HM.insert k n m, var)
    Nothing -> do
        n@(T2 var _) <- newTVarIO 0 >>= newUpdateStream
        return (HM.insert k n m, var)
  where
    checkStream :: T2 (TVar Int) (Async ()) -> RIO Env (T2 (TVar Int) (Async ()))
    checkStream (T2 var a) = poll a >>= \case
        Nothing -> return (T2 var a)
        Just (Left _) -> newUpdateStream var -- TODO logging, throttling
        Just (Right _) -> newUpdateStream var

    newUpdateStream :: TVar Int -> RIO Env (T2 (TVar Int) (Async ()))
    newUpdateStream var = T2 var
        <$!> async (updateStream (_updateKeyChainId k) var)

    -- TODO:
    --
    -- We don't reap old entries from the map. That's fine since the maximum
    -- number of entries is bounded by the number of base urls times the number
    -- of chains.
    --
    -- We could add a counter that would reap the map from stall streams every
    -- nth time getUpdateVar.

    -- We don't restart streams automatically, in order to save connections.
    -- Thus there is a risk that a stream dies while a mining loop is subscribed
    -- to it. We solve this by preempting the loop if we haven't seen an update
    -- after 2 times the block time.

withPreemption :: UpdateMap -> UpdateKey -> RIO Env a -> RIO Env (Either () a)
withPreemption m k inner = do
    var <- getUpdateVar m k
    race (awaitChange var) inner
  where
    awaitChange var = do
        cur <- readTVarIO var
        timeoutVar <- registerDelay 60000000 -- 1 minute -- FIXME this should be 2*block time
        atomically $ do
            isTimeout <- readTVar timeoutVar
            isUpdate <- (/= cur) <$> readTVar var
            unless (isTimeout || isUpdate) retrySTM

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
        }

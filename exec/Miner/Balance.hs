{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Miner.Balance ( getBalances ) where

import           Data.Aeson (Value(..))
import           Data.Decimal (Decimal, roundTo)
import           Data.Default (def)
import qualified Data.DList as D
import           Data.Semigroup.Foldable
import           Data.These (These(..))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           RIO
import qualified RIO.List as L
import qualified RIO.NonEmpty.Partial as NEL
import qualified RIO.Text as T
import           Servant.Client
import           Text.Printf (printf)

-- internal modules

import           Chainweb.ChainId (chainIdFromText)
import           Chainweb.Pact.RestAPI.Client (pactLocalApiClient)
import           Chainweb.RestAPI.NodeInfo (NodeInfo(..), NodeInfoApi)
import           Chainweb.Utils (sshow)
import           Miner.Types (tlsSettings)
import           Pact.ApiReq
import qualified Pact.Types.Command as P (CommandResult(..), PactResult(..))
import           Pact.Types.Exp (Literal(..))
import           Pact.Types.PactValue (PactValue(..))

getBalances :: BaseUrl -> Text -> IO ()
getBalances url mi = do
    balanceStmts <- newManager (mkManagerSettings tlsSettings Nothing) >>= go . cenv
    case balanceStmts of
      These errors balances -> do
        printf "-- Retrieved Balances -- \n"
        forM_ balances printer
        printf "-- Errors --\n"
        forM_ errors errPrinter
      This errors -> do
        printf "-- Errors --\n"
        forM_ errors errPrinter
      That balances -> do
        printf "-- Retrieved Balances -- \n"
        total <- foldM printBalance 0 balances
        printf $ "Total   => ₭" <> sshow (roundTo 12 total) <> "\n"
  where
    printer (a, b) = printf $ T.unpack (toBalanceMsg a b) <> ".\n"
    errPrinter (a,b) = printf $ T.unpack (toErrMsg a b) <> ".\n"
    printBalance :: Decimal -> (Text, Decimal) -> IO Decimal
    printBalance tot (c, bal) = tot + bal <$ printf (T.unpack (toBalanceMsg c bal) <> "\n")
    cenv m = ClientEnv m url Nothing
    mConc as f = runConcurrently $ foldMap1 (Concurrently . f) as

    go :: ClientEnv -> IO (These (D.DList (Text, LocalCmdError)) (D.DList (Text, Decimal)))
    go env = do
      NodeInfo v _ cs _ <-
         runClientM (client (RIO.Proxy @NodeInfoApi)) env >>= either (throwString . show) return
      mConc (NEL.fromList $ L.sort cs) $ \cidtext -> do
          c <- chainIdFromText cidtext
          cmd <-
            mkExec (printf "(coin.get-balance \"%s\")" mi) Null def mempty Nothing Nothing
          toLocalResult cidtext <$> runClientM (pactLocalApiClient v c cmd) env

    toLocalResult c r =
      case r of
        Right res -> convertResult c $ P._crResult res
        Left l    -> This $ D.singleton (c, Client l)

    convertResult c (P.PactResult result) =
       case result of
        Right (PLiteral (LDecimal bal)) -> That $ D.singleton (c, bal)
        Left perr -> This $ D.singleton (c, LookupError (sshow perr))
        Right a -> This $ D.singleton (c, PactResponseError (sshow a))

toErrMsg :: Text -> LocalCmdError -> Text
toErrMsg c (Client err) =
    "Client error on chain "
    <> c
    <> ": "
    <> sshow err
toErrMsg c (LookupError err)
    = "Balance lookup error on chain: "
    <> c
    <> ": "
    <> err

toErrMsg c (PactResponseError err) = mconcat
    [ "Pact result error on chain: "
    , sshow c
    , ": "
    , sshow err
    , ". This should never happen. Please raise an issue at "
    , "https://github.com/kadena-io/chainweb-node/issues."
    ]

toBalanceMsg :: Text -> Decimal -> Text
toBalanceMsg cidtext bal =
    "Chain "
    <> cidtext
    <> " => "
    <> "₭"
    <> sshow (roundTo 12 bal)

data LocalCmdError
  = Client ClientError
  | LookupError Text
  | PactResponseError Text

module Utils where

import Options.Applicative

import Network.Connection

import RIO

import Servant.Client

-- chainweb imports

import Chainweb.HostAddress
import Chainweb.Utils

data LocalCmdError
  = Client ClientError
  | LookupError Text
  | PactResponseError Text

data OtherCommand =
  Keys | Balance BaseUrl Text

keysOpts :: Parser OtherCommand
keysOpts = pure Keys

balancesOpts :: Parser OtherCommand
balancesOpts = Balance <$> pUrl <*> pMinerName
  where
    pMinerName =
      textOption (long "miner-account" <> help "Coin Contract account name of Miner")

pUrl :: Parser BaseUrl
pUrl = hostAddressToBaseUrl Https <$> hadd
  where
    hadd :: Parser HostAddress
    hadd = textOption
        (long "node" <> metavar "<HOSTNAME:PORT>"
        <> help "Remote address of Chainweb Node to send mining results to")

-- | This allows this code to accept the self-signed certificates from
-- `chainweb-node`.
--
ss :: TLSSettings
ss = TLSSettingsSimple True True True

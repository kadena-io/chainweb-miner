{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Miner.Types
  ( -- * Runtime Environment
    Env(..)
    -- * CLI Flags
  , ClientArgs(..)
  , pCommand
  , Command(..)
  , CPUEnv(..)
  , GPUEnv(..)
  , OtherCommand(..)
    -- * miscellaneous
  , ss
  ) where

import           Chainweb.Utils (textOption)
import           Data.Default (def)
import           Data.Generics.Product.Fields (field)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Data.Tuple.Strict (T2(..))
import           Network.Connection
import           Network.HTTP.Client hiding (Proxy(..), responseBody)
import           Options.Applicative
import           RIO
import           RIO.Char (isHexDigit)
import qualified RIO.Set as S
import qualified RIO.Text as T
import           Servant.Client
import qualified System.Random.MWC as MWC

-- internal modules

import           Chainweb.HostAddress (HostAddress, hostAddressToBaseUrl)
import           Chainweb.Miner.Pact (Miner(..), MinerKeys(..))
import           Chainweb.Version (ChainId, ChainwebVersion)
import qualified Pact.Types.Term as P

--------------------------------------------------------------------------------
-- Runtime Environment

data Env = Env
    { envGen         :: !MWC.GenIO
    , envMgr         :: !Manager
    , envLog         :: !LogFunc
    , envCmd         :: !Command
    , envArgs        :: !ClientArgs
    , envHashes      :: IORef Word64
    , envSecs        :: IORef Word64
    , envLastSuccess :: IORef POSIXTime
    , envUrls        :: IORef (NonEmpty (T2 BaseUrl ChainwebVersion)) }
    deriving stock (Generic)

instance HasLogFunc Env where
    logFuncL = field @"envLog"

--------------------------------------------------------------------------------
-- CLI Flags

-- | Result of parsing commandline flags.
--
data ClientArgs = ClientArgs
    { ll           :: !LogLevel
    , coordinators :: ![BaseUrl]
    , miner        :: !Miner
    , chainid      :: !(Maybe ChainId) }
    deriving stock (Generic)

-- | The top-level git-style CLI "command" which determines which mining
-- paradigm to follow.
--
data Command = CPU CPUEnv ClientArgs | GPU GPUEnv ClientArgs | Otherwise OtherCommand

newtype CPUEnv = CPUEnv { cores :: Word16 }

data GPUEnv = GPUEnv
    { envMinerPath :: Text
    , envMinerArgs :: [Text]
    } deriving stock (Generic)

pClientArgs :: Parser ClientArgs
pClientArgs = ClientArgs <$> pLog <*> some pUrl <*> pMiner <*> pChainId

pCommand :: Parser Command
pCommand = hsubparser
    (  command "cpu" (info cpuOpts (progDesc "Perform multicore CPU mining"))
    <> command "gpu" (info gpuOpts (progDesc "Perform GPU mining"))
    <> command "keys" (info (Otherwise <$> keysOpts) (progDesc "Generate public/private key pair"))
    <> command "balance" (info (Otherwise <$> balancesOpts) (progDesc "Get balances on all chains"))
    )

pMinerPath :: Parser Text
pMinerPath = textOption
    (long "miner-path" <> help "Path to chainweb-gpu-miner executable")

pMinerArgs :: Parser [Text]
pMinerArgs = T.words <$> pMinerArgs0
  where
    pMinerArgs0 :: Parser T.Text
    pMinerArgs0 = textOption
        (long "miner-args" <> value "" <> help "Extra miner arguments")

pGpuEnv :: Parser GPUEnv
pGpuEnv = GPUEnv <$> pMinerPath <*> pMinerArgs

gpuOpts :: Parser Command
gpuOpts = liftA2 GPU pGpuEnv pClientArgs

cpuOpts :: Parser Command
cpuOpts = liftA2 (CPU . CPUEnv) pCores pClientArgs

pCores :: Parser Word16
pCores = option auto
    (long "cores" <> metavar "COUNT" <> value 1
     <> help "Number of CPU cores to use (default: 1)")

pLog :: Parser LogLevel
pLog = option (eitherReader l)
    (long "log-level" <> metavar "debug|info|warn|error" <> value LevelInfo
    <> help "The minimum level of log messages to display (default: info)")
  where
    l :: String -> Either String LogLevel
    l "debug" = Right LevelDebug
    l "info"  = Right LevelInfo
    l "warn"  = Right LevelWarn
    l "error" = Right LevelError
    l _       = Left "Must be one of debug|info|warn|error"

pUrl :: Parser BaseUrl
pUrl = hostAddressToBaseUrl Https <$> hadd
  where
    hadd :: Parser HostAddress
    hadd = textOption
        (long "node" <> metavar "<HOSTNAME:PORT>"
        <> help "Remote address of Chainweb Node to send mining results to")

pChainId :: Parser (Maybe ChainId)
pChainId = optional $ textOption
    (long "chain" <> metavar "CHAIN-ID"
     <> help "Prioritize work requests for a specific chain")

pMiner :: Parser Miner
pMiner = Miner
    <$> strOption (long "miner-account" <> help "Coin Contract account name of Miner")
    <*> (MinerKeys <$> pks)
  where
    pks :: Parser P.KeySet
    pks = P.KeySet <$> fmap S.fromList (some pKey) <*> pPred

pKey :: Parser P.PublicKey
pKey = option k (long "miner-key"
    <> help "Public key of the account to send rewards (can pass multiple times)")
  where
    k :: ReadM P.PublicKey
    k = eitherReader $ \s -> do
        unless (length s == 64 && all isHexDigit s)
            . Left $ "Public Key " <> s <> " is not valid."
        Right $ fromString s

pPred :: Parser P.Name
pPred = (\s -> P.Name $ P.BareName s def) <$>
    strOption (long "miner-pred" <> value "keys-all" <> help "Keyset predicate")

data OtherCommand =
  Keys | Balance BaseUrl Text

keysOpts :: Parser OtherCommand
keysOpts = pure Keys

balancesOpts :: Parser OtherCommand
balancesOpts = Balance <$> pUrl <*> pMinerName
  where
    pMinerName =
      textOption (long "miner-account" <> help "Coin Contract account name of Miner")

-- | This allows this code to accept the self-signed certificates from
-- `chainweb-node`.
--
ss :: TLSSettings
ss = TLSSettingsSimple True True True

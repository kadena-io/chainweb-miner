{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Miner.OpenCL(
  queryAllOpenCLDevices,
  openCLMiner,
  OpenCLPlatform(..),
  OpenCLDevice(..)
) where

import           Control.Exception.Safe
import           Control.Parallel.OpenCL
import           Data.Bits
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import           Data.Char
import           Data.Foldable
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time(diffUTCTime, getCurrentTime)
import           Data.Tuple.Strict(T2(..))
import qualified Data.List as L
import           Data.Word
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           GHC.Show(intToDigit)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           System.Random

import           Chainweb.Miner.Core

import           Miner.Types(OpenCLEnv(..))

data OpenCLPlatform = OpenCLPlatform
  { platformId :: !CLPlatformID
  , platformName :: !Text
  , platformVersion :: !Text
  , platformVendor :: !Text
  , platformExtensions :: ![Text]
  , platformDevices :: ![OpenCLDevice]
  } deriving Show

data OpenCLDevice = OpenCLDevice
  { deviceId :: !CLDeviceID
  , devicePlatformId :: !CLPlatformID
  , deviceName :: !Text
  , deviceVendor :: !Text
  , deviceVersion :: !Text
  , deviceTypes :: ![CLDeviceType]
  , deviceAvailable :: !Bool
  , deviceExtensions :: ![Text]
  , deviceAddressBits :: !CLuint
  , deviceGlobalMemSize :: !CLulong
  , deviceGlobalCacheSize :: !CLulong
  , deviceCacheLineSize :: !CLuint
  , deviceLocalMemSize :: !CLulong
  , deviceMaxClock :: !CLuint
  , deviceComputeUnits :: !CLuint
  , deviceMaxWorkGroupSize :: !CSize
  , deviceMaxWorkItemSizes :: ![CSize]
  } deriving Show

data OpenCLWork = OpenCLWork
  { workContext :: !CLContext
  , workQueues :: ![OpenCLWorkQueue]
  , workSource :: !Text
  , workProgram :: !CLProgram
  , workKernel :: !CLKernel
  , workResultBuf :: !CLMem
  } deriving Show

data OpenCLWorkQueue = OpenCLWorkQueue
  { workDevice :: !OpenCLDevice
  , workQueue :: !CLCommandQueue
  } deriving Show

instance PP.Pretty OpenCLPlatform where
  prettyList = pList "Platform"
  pretty (OpenCLPlatform _ _ version _ _ devices) =
    text version <> PP.hardline <> PP.prettyList devices <> PP.hardline

instance PP.Pretty OpenCLDevice where
  prettyList = pList "Device"
  pretty (OpenCLDevice _ _ name _ _ tpe ava _ addressBits globalMemSize globalCacheSize _ localMemSize maxClock computeUnits workGroupSize itemSize) =
    text name <> PP.hardline <>
      PP.vsep [
        text "Max Clock Speed:     " <> integralDoc maxClock <> text " MHz",
        text "Total Memory:        " <> docBytes globalMemSize,
        text "Local Memory:        " <> docBytes localMemSize,
        text "Total Cache:         " <> docBytes globalCacheSize,
        text "Compute Units:       " <> integralDoc computeUnits,
        text "Max Work Group Size: " <> integralDoc workGroupSize,
        text "Item sizes:          " <> PP.encloseSep PP.empty PP.empty PP.comma (integralDoc <$> itemSize),
        text "Address Space:       " <> integralDoc addressBits <> text " Bits",
        text "Type:                " <> PP.text (show tpe),
        text "Status:              " <> available ava
      ] <> PP.hardline
   where
     available :: Bool -> PP.Doc
     available True = PP.green $ text "Available"
     available False = PP.red $ text "Unavailable"

buildOpenCLPlatform :: CLPlatformID -> IO OpenCLPlatform
buildOpenCLPlatform pId = do
    name <- platformInfo CL_PLATFORM_NAME
    version <- platformInfo CL_PLATFORM_VERSION
    vendor <- platformInfo CL_PLATFORM_VENDOR
    extensions <- T.splitOn " " <$> platformInfo CL_PLATFORM_EXTENSIONS
    deviceIds <- clGetDeviceIDs pId CL_DEVICE_TYPE_ALL
    devices <- traverse (buildDevice pId) deviceIds
    pure $ OpenCLPlatform pId name version vendor extensions devices
    where
    platformInfo prop = T.pack <$> clGetPlatformInfo pId prop

buildDevice :: CLPlatformID -> CLDeviceID -> IO OpenCLDevice
buildDevice pId dId = do
  name <- T.pack <$> clGetDeviceName dId
  version <- T.pack <$> clGetDeviceVersion dId
  vendor <- T.pack <$> clGetDeviceVendor dId
  tpe <- clGetDeviceType dId
  available <- clGetDeviceAvailable dId
  extensions <- T.splitOn " " . T.pack <$> clGetDeviceExtensions dId
  addressBits <- clGetDeviceAddressBits dId
  globalMemSize <- clGetDeviceGlobalMemSize dId
  globalCacheSize <- clGetDeviceGlobalMemCacheSize dId
  cacheLineSize <- clGetDeviceGlobalMemCachelineSize dId
  localMemSize <- clGetDeviceLocalMemSize dId
  maxClock <- clGetDeviceMaxClockFrequency dId
  computeUnits <- clGetDeviceMaxComputeUnits dId
  workGroupSize <- clGetDeviceMaxWorkGroupSize dId
  itemSize <- clGetDeviceMaxWorkItemSizes dId
  pure $ OpenCLDevice dId pId name vendor version tpe available extensions addressBits globalMemSize globalCacheSize cacheLineSize localMemSize maxClock computeUnits workGroupSize itemSize

queryAllOpenCLDevices :: IO [OpenCLPlatform]
queryAllOpenCLDevices = do
  platformIds <- clGetPlatformIDs
  traverse buildOpenCLPlatform platformIds

text :: Text -> PP.Doc
text = PP.text <$> T.unpack

data ByteMagnitude = B | K | M | G

docBytes :: Integral a => a -> PP.Doc
docBytes n = reduceBytes (fromIntegral n :: Double) B
  where
    reduceBytes :: Double -> ByteMagnitude -> PP.Doc
    reduceBytes a B = if a > 1024 then reduceBytes (a / 1024) K else (PP.double . round2) a <> text " B"
    reduceBytes a K = if a > 1024 then reduceBytes (a / 1024) M else (PP.double . round2) a <> text " KB"
    reduceBytes a M = if a > 1024 then reduceBytes (a / 1024) G else (PP.double . round2) a <> text " MB"
    reduceBytes a G = (PP.double . round2) a <> text " GB"

    round2 :: Double -> Double
    round2 a = fromIntegral (round (a * 100) :: Integer) / 100

integralDoc :: (Integral a) => a -> PP.Doc
integralDoc = PP.integer <$> toInteger

pList :: PP.Pretty a => Text -> [a] -> PP.Doc
pList prefix as = PP.vsep $ indexedDoc <$> zipped
 where
  indexes = L.findIndices (const True) as
  zipped = L.zip indexes as
  prefixDoc = text prefix
  numberedDoc idx = prefixDoc <> text " #" <> integralDoc idx
  nestedDoc a = PP.nest 6 (PP.pretty a)
  indexedDoc (idx, a) = numberedDoc idx <> text ": " <> nestedDoc a

createOpenCLContext :: [OpenCLDevice] -> IO CLContext
createOpenCLContext devices = clCreateContext (CL_CONTEXT_PLATFORM . devicePlatformId <$> devices) (deviceId <$> devices) putStrLn

createOpenCLProgram :: CLContext -> Text -> IO CLProgram
createOpenCLProgram ctx txt = clCreateProgramWithSource ctx $ T.unpack txt

joinArgs :: [Text] -> String
joinArgs = T.unpack . T.unwords

buildOpenCLProgram :: CLProgram -> [OpenCLDevice] -> [Text] -> IO CLProgram
buildOpenCLProgram prog devices args = do
  res <- try (clBuildProgram prog (deviceId <$> devices) (joinArgs args))
  case res of
    Left (err :: SomeException) -> do
        -- TODO: logger
        putStrLn =<< clGetProgramBuildLog prog (deviceId $ head $ devices)
        throw err
    Right _ -> pure ()
  pure prog

createOpenCLKernel :: CLProgram -> Text -> IO CLKernel
createOpenCLKernel prog name = clCreateKernel prog $ T.unpack name

createOpenCLWorkQueue :: CLContext -> [CLCommandQueueProperty] -> OpenCLDevice -> IO OpenCLWorkQueue
createOpenCLWorkQueue ctx props dev = OpenCLWorkQueue dev <$> clCreateCommandQueue ctx (deviceId dev) props

prepareOpenCLWork :: Text -> [OpenCLDevice] -> [Text] -> Text -> IO OpenCLWork
prepareOpenCLWork source devs args kernelName = do
  context <- createOpenCLContext devs
  program <- createOpenCLProgram context source
  builtProgram <- buildOpenCLProgram program devs args
  kernel <- createOpenCLKernel builtProgram kernelName
  queues <- traverse (createOpenCLWorkQueue context []) devs
  resultBuf <- clCreateBuffer context [CL_MEM_WRITE_ONLY] ((8::Int),nullPtr)
  clSetKernelArgSto kernel 1 resultBuf
  pure $ OpenCLWork context queues source builtProgram kernel resultBuf

bsToWord32s :: ByteString -> [Word32]
bsToWord32s = word32s . BS.unpack
  where
  word32s :: [Word8] -> [Word32]
  word32s [] = []
  word32s (a:b:c:d:xs) = roll [a,b,c,d] : word32s xs
  word32s _ = error "input was not possible to split into Word32's"

  roll :: [Word8] -> Word32
  roll   = foldr unstep 0
    where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

bsToWord64s :: ByteString -> [Word64]
bsToWord64s = word64s . BS.unpack
  where
  word64s :: [Word8] -> [Word64]
  word64s [] = []
  word64s (a:b:c:d:e:f:g:h:xs) = roll [a,b,c,d,e,f,g,h] : word64s xs
  word64s _ = error "input was not possible to split into Word64's"

  roll :: [Word8] -> Word64
  roll   = foldr unstep 0
    where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

-- | Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the character representation specified by the second.
showIntAtBase :: (Integral a, Show a) => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toChr n0 r0
  | base <= 1 = errorWithoutStackTrace ("Numeric.showIntAtBase: applied to unsupported base " ++ show base)
  | n0 <  0   = errorWithoutStackTrace ("Numeric.showIntAtBase: applied to negative number " ++ show n0)
  | otherwise = showIt (quotRem n0 base) r0
   where
    showIt (n,d) r = seq c $ -- stricter than necessary
      case n of
        0 -> r'
        _ -> showIt (quotRem n base) r'
     where
      c  = toChr (fromIntegral d)
      r' = c : r

-- | Show /non-negative/ 'Integral' numbers in base 16.
showHex :: (Integral a,Show a) => a -> String
showHex n = toUpper <$> showIntAtBase 16 intToDigit n []

targetBytesToOptions :: Int -> TargetBytes -> HeaderBytes -> [Text]
targetBytesToOptions wss (TargetBytes (bsToWord64s -> targetHash)) (HeaderBytes (bsToWord32s -> blockData)) =
  "-Werror" : ("-DWORKSET_SIZE=" <> T.pack (show wss)) : blockDataOptions ++ targetHashOptions
  where
  blockDataOptions = [0..80-1] >>= \i ->
    if i == 0 || i == 1 then []
    else
        let value =
                if i < 71 then blockData !! i
                else 0
        in [
            "-DB" <> T.pack (show (i`div`16)) <> T.pack (showHex (i `mod` 16)) <> "=" <> T.pack (show value) <> "U"
           ]
  targetHashOptions = [0..4-1] >>= \i ->
    let j = chr $ ord 'A' + (3 - i)
    in ["-D" <> T.pack (show j) <> "0=" <> T.pack (show (targetHash !! i)) <> "UL"]

run :: OpenCLEnv -> TargetBytes -> HeaderBytes -> Text -> OpenCLDevice -> IO Word64 -> IO MiningResult
run cfg target header src device genNonce = do
  startTime <- getCurrentTime
  let !args = targetBytesToOptions (workSetSize cfg) target header
  let kernelName = "search_nonce"
  work <- prepareOpenCLWork src [device] args kernelName
  offsetP <- calloc :: IO (Ptr CSize)
  resP <- calloc :: IO (Ptr Word64)
  !(T2 end steps) <- doIt offsetP resP work (1::Int)
  endTime <- getCurrentTime
  let numNonces = globalSize cfg * 64 * steps
  let !result = MiningResult
        (BSL.toStrict $ BSB.toLazyByteString $ BSB.word64LE end)
        (fromIntegral numNonces)
        (fromIntegral numNonces `div` max 1 (round (endTime `diffUTCTime` startTime)))
        mempty
  _ <- clReleaseMemObject (workResultBuf work)
  _ <- clReleaseKernel (workKernel work)
  _ <- clReleaseProgram (workProgram work)
  traverse_ clReleaseCommandQueue (workQueue <$> workQueues work)
  _ <- clReleaseContext (workContext work)
  free offsetP
  free resP
  pure result
  where
  doIt offsetP resP !work@(OpenCLWork _ [queue] _ _ kernel resultBuf) !n = do
        nonce <- genNonce
        clSetKernelArgSto kernel 0 nonce
        e1 <- clEnqueueWriteBuffer (workQueue queue) resultBuf True (0::Int) 8 (castPtr resP) []
        e2 <- clEnqueueNDRangeKernel (workQueue queue) kernel [globalSize cfg] [localSize cfg] [e1]
        e3 <- clEnqueueReadBuffer (workQueue queue) resultBuf True (0::Int) 8 (castPtr resP) [e2]
        _ <- clFinish (workQueue queue)
        -- required to avoid leaking everything else
        traverse_ clReleaseEvent [e1,e2,e3]
        !res <- peek resP
        if res == 0 then do
          doIt offsetP resP work (n+1)
        else do
          return $ T2 res n
  doIt _ _ _ _ = error "using multiple devices at once is currently unsupported"

openCLMiner :: OpenCLEnv -> OpenCLDevice -> TargetBytes -> HeaderBytes -> IO MiningResult
openCLMiner cfg device t h = do
  src <- T.readFile "kernel.cl"
  run cfg t h src device randomIO

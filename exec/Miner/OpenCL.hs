{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Miner.OpenCL
  ( queryAllOpenCLDevices
  , openCLMiner
  , OpenCLPlatform(..)
  , OpenCLDevice(..)
  ) where

import qualified Control.Parallel.OpenCL as O
import           Data.Bits
import qualified Data.ByteString.Builder as BSB
import           Data.Time (diffUTCTime, getCurrentTime)
import           Data.Tuple.Strict (T2(..))
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           RIO
import qualified RIO.ByteString as BS
import qualified RIO.ByteString.Lazy as BL
import           RIO.Char (ord)
import           RIO.Char.Partial (chr)
import qualified RIO.List as L
import           RIO.List.Partial ((!!))
import qualified RIO.Text as T
import           RIO.Text.Partial (splitOn)
import           System.Random
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Printf (printf)

-- internal modules

import           Chainweb.Miner.Core
import           Miner.Types (OpenCLEnv(..))

---

data OpenCLPlatform = OpenCLPlatform
    { platformId         :: !O.CLPlatformID
    , platformName       :: !Text
    , platformVersion    :: !Text
    , platformVendor     :: !Text
    , platformExtensions :: ![Text]
    , platformDevices    :: ![OpenCLDevice] }

data OpenCLDevice = OpenCLDevice
    { deviceId               :: !O.CLDeviceID
    , devicePlatformId       :: !O.CLPlatformID
    , deviceName             :: !Text
    , deviceVendor           :: !Text
    , deviceVersion          :: !Text
    , deviceTypes            :: ![O.CLDeviceType]
    , deviceAvailable        :: !Bool
    , deviceExtensions       :: ![Text]
    , deviceAddressBits      :: !O.CLuint
    , deviceGlobalMemSize    :: !O.CLulong
    , deviceGlobalCacheSize  :: !O.CLulong
    , deviceCacheLineSize    :: !O.CLuint
    , deviceLocalMemSize     :: !O.CLulong
    , deviceMaxClock         :: !O.CLuint
    , deviceComputeUnits     :: !O.CLuint
    , deviceMaxWorkGroupSize :: !CSize
    , deviceMaxWorkItemSizes :: ![CSize] } deriving (Show)

data OpenCLWork = OpenCLWork
    { workContext   :: !O.CLContext
    , workQueues    :: ![OpenCLWorkQueue]
    , _workSource   :: !Text
    , workProgram   :: !O.CLProgram
    , workKernel    :: !O.CLKernel
    , workResultBuf :: !O.CLMem }

data OpenCLWorkQueue = OpenCLWorkQueue
    { _workDevice :: !OpenCLDevice
    , workQueue   :: !O.CLCommandQueue }

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
     available True  = PP.green $ text "Available"
     available False = PP.red $ text "Unavailable"

buildOpenCLPlatform :: O.CLPlatformID -> IO OpenCLPlatform
buildOpenCLPlatform pId = do
    name <- platformInfo O.CL_PLATFORM_NAME
    version <- platformInfo O.CL_PLATFORM_VERSION
    vendor <- platformInfo O.CL_PLATFORM_VENDOR
    extensions <- splitOn " " <$> platformInfo O.CL_PLATFORM_EXTENSIONS
    deviceIds <- O.clGetDeviceIDs pId O.CL_DEVICE_TYPE_ALL
    devices <- traverse (buildDevice pId) deviceIds
    pure $ OpenCLPlatform pId name version vendor extensions devices
  where
    platformInfo prop = T.pack <$> O.clGetPlatformInfo pId prop

buildDevice :: O.CLPlatformID -> O.CLDeviceID -> IO OpenCLDevice
buildDevice pId dId = do
  name <- T.pack <$> O.clGetDeviceName dId
  version <- T.pack <$> O.clGetDeviceVersion dId
  vendor <- T.pack <$> O.clGetDeviceVendor dId
  tpe <- O.clGetDeviceType dId
  available <- O.clGetDeviceAvailable dId
  extensions <- splitOn " " . T.pack <$> O.clGetDeviceExtensions dId
  addressBits <- O.clGetDeviceAddressBits dId
  globalMemSize <- O.clGetDeviceGlobalMemSize dId
  globalCacheSize <- O.clGetDeviceGlobalMemCacheSize dId
  cacheLineSize <- O.clGetDeviceGlobalMemCachelineSize dId
  localMemSize <- O.clGetDeviceLocalMemSize dId
  maxClock <- O.clGetDeviceMaxClockFrequency dId
  computeUnits <- O.clGetDeviceMaxComputeUnits dId
  workGroupSize <- O.clGetDeviceMaxWorkGroupSize dId
  itemSize <- O.clGetDeviceMaxWorkItemSizes dId
  pure $ OpenCLDevice dId pId name vendor version tpe available extensions addressBits globalMemSize globalCacheSize cacheLineSize localMemSize maxClock computeUnits workGroupSize itemSize

queryAllOpenCLDevices :: IO [OpenCLPlatform]
queryAllOpenCLDevices = do
  platformIds <- O.clGetPlatformIDs
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

createOpenCLContext :: [OpenCLDevice] -> IO O.CLContext
createOpenCLContext devices =
    O.clCreateContext props (deviceId <$> devices) (BL.putStrLn . fromString)
  where
    props = O.CL_CONTEXT_PLATFORM . devicePlatformId <$> devices

createOpenCLProgram :: O.CLContext -> Text -> IO O.CLProgram
createOpenCLProgram ctx txt = O.clCreateProgramWithSource ctx $ T.unpack txt

joinArgs :: [Text] -> String
joinArgs = T.unpack . T.unwords

buildOpenCLProgram :: O.CLProgram -> [OpenCLDevice] -> [Text] -> IO O.CLProgram
buildOpenCLProgram prog devices args = do
  res <- try (O.clBuildProgram prog (deviceId <$> devices) (joinArgs args))
  case res of
    Left (err :: SomeException) ->
        -- TODO: logger
        -- putStrLn =<< clGetProgramBuildLog prog (deviceId $ head $ devices)
        throwM err
    Right _ -> pure ()
  pure prog

createOpenCLKernel :: O.CLProgram -> Text -> IO O.CLKernel
createOpenCLKernel prog name = O.clCreateKernel prog $ T.unpack name

createOpenCLWorkQueue
    :: O.CLContext
    -> [O.CLCommandQueueProperty]
    -> OpenCLDevice
    -> IO OpenCLWorkQueue
createOpenCLWorkQueue ctx props dev =
    OpenCLWorkQueue dev <$> O.clCreateCommandQueue ctx (deviceId dev) props

prepareOpenCLWork :: Text -> [OpenCLDevice] -> [Text] -> Text -> IO OpenCLWork
prepareOpenCLWork source devs args kernelName = do
  context <- createOpenCLContext devs
  program <- createOpenCLProgram context source
  builtProgram <- buildOpenCLProgram program devs args
  kernel <- createOpenCLKernel builtProgram kernelName
  queues <- traverse (createOpenCLWorkQueue context []) devs
  resultBuf <- O.clCreateBuffer context [O.CL_MEM_WRITE_ONLY] (8 :: Int, nullPtr)
  O.clSetKernelArgSto kernel 1 resultBuf
  pure $ OpenCLWork context queues source builtProgram kernel resultBuf

bsToWord32s :: ByteString -> [Word32]
bsToWord32s = word32s . BS.unpack
  where
  word32s :: [Word8] -> [Word32]
  word32s []           = []
  word32s (a:b:c:d:xs) = roll [a,b,c,d] : word32s xs
  word32s _            = error "input was not possible to split into Word32's"

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
            "-DB" <> T.pack (show (i`div`16)) <> T.pack (printf "%h" (i `mod` 16)) <> "=" <> T.pack (show value) <> "U"
           ]
  targetHashOptions = [0..4-1] >>= \i ->
    let j = chr $ ord 'A' + (3 - i)
    in [T.snoc "-D" j <> "0=" <> T.pack (show (targetHash !! i)) <> "UL"]

run :: OpenCLEnv -> TargetBytes -> HeaderBytes -> Text -> OpenCLDevice -> IO Word64 -> IO MiningResult
run cfg target header src device genNonce = do
    startTime <- getCurrentTime
    let !args = targetBytesToOptions (workSetSize cfg) target header
    let kernelName = "search_nonce"
    work <- prepareOpenCLWork src [device] args kernelName
    offsetP <- calloc :: IO (Ptr CSize)
    resP <- calloc :: IO (Ptr Word64)
    T2 end steps <- doIt offsetP resP work (1::Int)
    endTime <- getCurrentTime
    let numNonces = globalSize cfg * 64 * steps
    let !result = MiningResult
          (BL.toStrict $ BSB.toLazyByteString $ BSB.word64LE end)
          (fromIntegral numNonces)
          (fromIntegral numNonces `div` max 1 (round (endTime `diffUTCTime` startTime)))
          mempty
    _ <- O.clReleaseMemObject (workResultBuf work)
    _ <- O.clReleaseKernel (workKernel work)
    _ <- O.clReleaseProgram (workProgram work)
    traverse_ O.clReleaseCommandQueue (workQueue <$> workQueues work)
    _ <- O.clReleaseContext (workContext work)
    free offsetP
    free resP
    pure result
  where
    doIt offsetP resP work@(OpenCLWork _ [queue] _ _ kernel resultBuf) !n = do
        nonce <- genNonce
        O.clSetKernelArgSto kernel 0 nonce
        e1 <- O.clEnqueueWriteBuffer (workQueue queue) resultBuf True (0::Int) 8 (castPtr resP) []
        e2 <- O.clEnqueueNDRangeKernel (workQueue queue) kernel [globalSize cfg] [localSize cfg] [e1]
        e3 <- O.clEnqueueReadBuffer (workQueue queue) resultBuf True (0::Int) 8 (castPtr resP) [e2]
        _ <- O.clFinish (workQueue queue)
        -- required to avoid leaking everything else
        traverse_ O.clReleaseEvent [e1,e2,e3]
        !res <- peek resP
        if | res == 0 -> doIt offsetP resP work (n+1)
           | otherwise -> return $ T2 res n
    doIt _ _ _ _ = error "using multiple devices at once is currently unsupported"

openCLMiner :: OpenCLEnv -> OpenCLDevice -> TargetBytes -> HeaderBytes -> IO MiningResult
openCLMiner cfg device t h = do
  src <- readFileUtf8 "kernel.cl"
  run cfg t h src device randomIO

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy hiding (length, putStrLn, empty)
import qualified Data.ByteString.Lazy as L (length)
import Data.Int
import Data.IntMap.Strict hiding (empty)
import qualified Data.IntMap.Strict as IM (empty)
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Text.Printf

{- This is a combination of the parts called "Frame", "Frame Address",
   and "Protocol header" in the documentation:
   http://lan.developer.lifx.com/docs/header-description -}
data Header
  = Header
    { hdrSize        :: !Word16
    , hdrOrigin      :: !Word8
    , hdrTagged      :: !Bool
    , hdrAddressable :: !Bool
    , hdrProtocol    :: !Word16
    , hdrSource      :: !Word32
    , hdrTarget      :: !Word64
    -- , hdrReserved48  :: !Word64
    -- , hdrReserved6   :: !Word8
    , hdrAckRequired :: !Bool
    , hdrResRequired :: !Bool
    , hdrSequence    :: !Word8
    -- , hdrReserved64  :: !Word64
    , hdrType        :: !Word16
    -- , hdrReserved16  :: !Word16
    } deriving Show

dfltHdr = dh { hdrSize = fromIntegral $ L.length $ encode dh }
  where dh = Header { hdrSize = 0
                    , hdrOrigin = 0
                    , hdrTagged = False
                    , hdrAddressable = True
                    , hdrProtocol = 1024
                    , hdrSource = 91376
                    , hdrTarget = 0
                    , hdrAckRequired = False
                    , hdrResRequired = False
                    , hdrSequence = 0
                    , hdrType = 0
                    }

dfltHdrSize = L.length $ encode dfltHdr

bProtocol    = 0
bAddressable = 12
bTagged      = 13
bOrigin      = 14

bResRequired = 0
bAckRequired = 1

bounds :: (Integral a, Bits a, Show a) => String -> Int -> a -> Put
bounds name n val =
  when (val >= limit) $ fail (name ++ ": " ++ show val ++ " >= " ++ show limit)
  where limit = bit n

bitBool :: Bits a => Int -> Bool -> a
bitBool _ False = zeroBits
bitBool n True = bit n

extract :: (Integral a, Bits a, Integral b) => a -> Int -> Int -> b
extract x n w = fromIntegral field
  where field = (x `shiftR` n) .&. mask
        mask = (bit w) - 1

instance Binary Header where
  put h = do
    -- "Frame"
    putWord16le $ hdrSize h
    let hOrg = fromIntegral $ hdrOrigin h
        hTag = hdrTagged h
        hAdd = hdrAddressable h
        hPro = hdrProtocol h
    bounds "origin" 2 hOrg
    bounds "protocol" 12 hPro
    putWord16le $
      (hPro `shiftL` bProtocol) +
      bitBool bAddressable hAdd +
      bitBool bTagged hTag +
      (hOrg `shiftL` bOrigin)
    putWord32le $ hdrSource h
    -- "Frame Address"
    putWord64le $ hdrTarget h
    putWord32le 0 -- Reserved48
    putWord16le 0
    let hAck = hdrAckRequired h
        hRes = hdrResRequired h
    putWord8 $ bitBool bResRequired hRes + bitBool bAckRequired hAck
    putWord8 $ hdrSequence h
    -- "Protocol Header"
    putWord64le 0 -- Reserved64
    putWord16le $ hdrType h
    putWord16le 0 -- Reserved16

  get = do
    h <- Header <$> getWord16le -- hdrSize
    otap <- getWord16le
    let hh = h (extract otap bOrigin 2)
               (testBit otap bTagged)
               (testBit otap bAddressable)
               (extract otap bProtocol 12)
    hhh <- hh <$> getWord32le -- hdrSource
              <*> getWord64le -- hdrTarget
    getWord32le -- Reserved48
    getWord16le
    ar <- getWord8
    let hhhh = hhh (testBit ar bAckRequired) (testBit ar bResRequired)
    hhhhh <- hhhh <$> getWord8 -- hdrSequence
    getWord64le -- Reserved64
    hhhhhh <- hhhhh <$> getWord16le -- hdrType
    getWord16le -- Reserved16
    return hhhhhh

class MessageType t where
  msgType :: t -> Word16

data GetService = GetService

instance MessageType GetService where
  msgType _ = 2

instance Binary GetService where
  put _ = return ()
  get = return GetService

data StateService
  = StateService
    { ssService :: !Word8
    , ssPort    :: !Word32
    } deriving Show

instance MessageType StateService where
  msgType _ = 3

instance Binary StateService where
  put x = do
    putWord8 $ ssService x
    putWord32le $ ssPort x

  get =
    StateService <$> getWord8 <*> getWord32le

data GetHostInfo = GetHostInfo

instance MessageType GetHostInfo where
  msgType _ = 12

instance Binary GetHostInfo where
  put _ = return ()
  get = return GetHostInfo

data StateHostInfo
  = StateHostInfo
    { shiSignal :: !Word32 -- Float; use reinterpret-cast package
    , shiTX :: !Word32
    , shiRX :: !Word32
    , shiMcuTemperature :: !Word16 -- Int16; use unsafe-coerce
    } deriving Show

instance MessageType StateHostInfo where
  msgType _ = 13

instance Binary StateHostInfo where
  put x = do
    putWord32le $ shiSignal x
    putWord32le $ shiTX x
    putWord32le $ shiRX x
    putWord16le $ shiMcuTemperature x

  get =
    StateHostInfo <$> getWord32le <*> getWord32le <*> getWord32le <*> getWord16le

serializeMsg :: (MessageType a, Binary a) => Header -> a -> ByteString
serializeMsg hdr payload = hdrBs `append` payloadBS
  where payloadBS = encode payload
        hsize = dfltHdrSize + L.length payloadBS
        hdr' = hdr { hdrType = msgType payload , hdrSize = fromIntegral hsize }
        hdrBs = encode hdr'

type Callback = InternalState -> SockAddr -> Header -> ByteString -> IO ()

data InternalState
  = InternalState
    { stSeq :: !Word8
    , stSource :: !Word32
    , stCallbacks :: IntMap Callback
    , stLog :: String -> IO ()
    , stSocket :: Socket
    }

dfltState = InternalState { stSeq = 0
                          , stSource = 37619
                          , stCallbacks = IM.empty
                          , stLog = putStrLn
                          , stSocket = undefined
                          }

newHdr :: InternalState -> (InternalState, Header)
newHdr st = (newSt, hdr)
  where seq = stSeq st
        newSt = st { stSeq = seq + 1 }
        hdr = dfltHdr { hdrSource = stSource st , hdrSequence = seq }

registerCallback :: InternalState -> Header -> Callback -> InternalState
registerCallback st hdr cb = st { stCallbacks = cbacks' }
  where cbacks = stCallbacks st
        seq = fromIntegral $ hdrSequence hdr
        cbacks' = insert seq cb cbacks

-- resorted to this weird thing to fix type errors
contortedDecode :: Binary a => ByteString -> (a, Either String Int64)
contortedDecode bs =
  case decodeOrFail bs of
   Left ( _ , _ , msg ) -> ( undefined , Left msg )
   Right ( lftovr , _ , payload ) -> ( payload , Right (L.length lftovr) )

checkHeaderFields :: (MessageType a, Binary a)
                     => Header -> ByteString
                     -> Either String a
checkHeaderFields hdr bs =
  let (payload, decodeResult) = contortedDecode bs
      typ = hdrType hdr
      expected = msgType payload
  in if typ /= expected
     then Left $ "expected type " ++ show expected ++ " but got " ++ show typ
     else case decodeResult of
           Left msg -> Left msg
           Right lftovr
             | lftovr /= 0 -> Left $ show lftovr ++ " bytes left over"
             | otherwise -> Right payload

wrapCallback :: (MessageType a, Binary a) => (Header -> a -> IO ()) -> Callback
wrapCallback cb st sa hdr bs = f $ checkHeaderFields hdr bs
  where f (Left msg) = stLog st $ msg ++ " (from " ++ show sa ++ ")"
        f (Right payload) = cb hdr payload

serviceUDP = 1

newtype Target = Target Word64

instance Show Target where
  show (Target x) = colonize $ printf "%012X" (x .&. 0xffffffffffff)
    where colonize [c1, c2] = [c1, c2]
          colonize (c1:c2:rest) = c1 : c2 : ':' : colonize rest

data Bulb = Bulb SockAddr Target deriving Show

wrapStateService :: (Bulb -> IO ()) -> Callback
wrapStateService cb st sa hdr bs = f $ checkHeaderFields hdr bs
  where f (Left msg) = stLog st (msg ++ frm)
        f (Right payload) = bulb (ssService payload) (ssPort payload)
        frm = " (from " ++ show sa ++ ")"
        bulb serv port
          | serv /= serviceUDP = stLog st $ "service: expected "
                                 ++ show serviceUDP ++ " but got "
                                 ++ show serv ++ frm
          | otherwise = cb $ Bulb (substPort sa port) (Target $ hdrTarget hdr)
        substPort (SockAddrInet _ ha) port = SockAddrInet (fromIntegral port) ha
        substPort other _ = other

wrapAndRegister :: (MessageType a, Binary a)
                   => InternalState -> Header
                   -> (Header -> a -> IO ())
                   -> InternalState
wrapAndRegister st hdr cb = registerCallback st hdr $ wrapCallback cb

newHdrAndCallback :: (MessageType a, Binary a)
                     => InternalState
                     -> (Header -> a -> IO ())
                     -> (InternalState, Header)
newHdrAndCallback st cb = (st'', hdr)
  where (st', hdr) = newHdr st
        st'' = wrapAndRegister st' hdr cb

newHdrAndCbDiscovery :: InternalState
                        -> (Bulb -> IO ())
                        -> (InternalState, Header)
newHdrAndCbDiscovery st cb = (st'', hdr)
  where (st', hdr) = newHdr st
        st'' = registerCallback st' hdr $ wrapStateService cb

runCallback :: InternalState -> SockAddr -> ByteString -> IO ()
runCallback st sa bs =
  case decodeOrFail bs of
   Left (_, _, msg) -> stLog st msg
   Right (bs', _, hdr) ->
     let hsz = fromIntegral (hdrSize hdr)
         len = L.length bs
         hsrc = hdrSource hdr
         ssrc = stSource st
         seq = fromIntegral (hdrSequence hdr)
         cbacks = stCallbacks st
         nuthin' _ _ _ _ = return ()
     in if hsz /= len
        then stLog st $ "length mismatch: " ++ show hsz ++ " /= " ++ show len
        else if hsrc /= ssrc
             then stLog st $ "source mismatch: " ++ show hsrc ++ " /= " ++ show ssrc
             else findWithDefault nuthin' seq cbacks st sa hdr bs'

sendMsg :: (MessageType a, Binary a)
           => InternalState -> Bulb -> Header -> a
           -> IO ()
sendMsg st (Bulb sa (Target targ)) hdr payload =
  sendManyTo (stSocket st) (toChunks pkt) sa
  where hdr' = hdr { hdrTarget = targ }
        pkt = serializeMsg hdr' payload

discovery :: InternalState -> (InternalState, ByteString)
discovery st = (st', bs)
  where (st', hdr) = newHdrAndCbDiscovery st cb
        hdr' = hdr { hdrTagged = True }
        bs = serializeMsg hdr' GetService
        cb bulb = putStrLn (show bulb)

ethMtu = 1500

main = do
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
  when (isSupportedSocketOption Broadcast) (setSocketOption sock Broadcast 1)
  let flags = [ AI_NUMERICHOST , AI_NUMERICSERV ]
      myHints = defaultHints { addrFlags = flags }
  (ai:_ ) <- getAddrInfo (Just myHints) (Just "192.168.11.255") (Just "56700")
  let bcast = addrAddress ai
      (st, pkt) = discovery dfltState
  sendManyTo sock (toChunks pkt) bcast
  (bs, sa) <- recvFrom sock ethMtu
  runCallback st sa $ fromStrict bs
  close sock

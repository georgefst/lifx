import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Int
import Data.Word

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
    }

bProtocol    = 0
bAddressable = 12
bTagged      = 13
bOrigin      = 14

bResRequired = 0
bAckRequired = 1

bounds :: (Integral a, Bits a) => Int -> a -> Put
bounds name bits val =
  when (val >= limit) $ fail (name ++ ": " ++ show val ++ " >= " ++ show bound)
  where limit = fromIntegral 1 `shiftL` bits

-- FIXME: bit
bitBool :: (Integral a, Bits a) => Int -> Bool -> a
bitBool _ False = fromIntegral 0
bitBool n True = fromIntegral 1 `shiftL` n

-- FIXME: testBit
boolBit :: (Integral a, Bits a) => a -> Int -> Bool
boolBit x n = (x .&. (1 `shiftL` n)) /= 0

instance Binary Header where
  put h = do
    -- "Frame"
    putWord16le $ hdrSize h
    let hOrg = hdrOrigin h
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
               (boolBit otap bTagged)
               (boolBit otap bAddressable)
               (extract otap bProtocol 12)
    hhh <- hh <$> getWord16le -- hdrSource
              <*> getWord64le -- hdrTarget
    getWord32le -- Reserved48
    getWord16le
    ar <- getWord8
    let hhhh = hhh (boolBit ar bAckRequired) (boolBit ar bResRequired)
    hhhhh <- hhhh <$> getWord8 -- hdrSequence
    getWord64 -- Reserved64
    hhhhhh <- hhhhh <$> getWord16le -- hdrType
    getWord16 -- Reserved16
    return hhhhhh

data StateService
  = StateService
    { ssService :: !Word8
    , ssPort    :: !Word32
    }

instance Binary StateService where
  put x = do
    putWord8 $ ssService x
    putWord32le $ ssPort x

  get = do
    StateService <$> getWord8 <*> getWord32le

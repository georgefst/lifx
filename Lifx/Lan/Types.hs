module Lifx.Lan.Types
    ( MessageType(..),
      Header(..),
      dfltHdr,
      dfltHdrSize,
      Waveform(..),
      HSBK(..) ) where

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Data.Binary
    ( Binary(..),
      putWord8,
      getWord8,
      encode )
import Data.Binary.Put ( putWord64be, putWord64le, putWord32le, putWord16le )
import Data.Binary.Get ( getWord64be, getWord64le, getWord32le, getWord16le )
import Data.Bits ( Bits(shiftL, testBit) )
import qualified Data.ByteString.Lazy as L ( length )
import Data.Word ( Word8, Word16, Word32, Word64 )

import Lifx.Lan.Util

data HSBK =
  HSBK
  { hue :: !Word16
  , saturation :: !Word16
  , brightness :: !Word16
  , kelvin :: !Word16
  } deriving Show

instance Binary HSBK where
  put x = do
    putWord16le $ hue x
    putWord16le $ saturation x
    putWord16le $ brightness x
    putWord16le $ kelvin x

  get = HSBK <$> getWord16le <*> getWord16le <*> getWord16le <*> getWord16le


data Waveform = Saw | Sine | HalfSine | Triangle | Pulse deriving (Show, Enum)


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
    putWord64be $ hdrTarget h
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
              <*> getWord64be -- hdrTarget
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

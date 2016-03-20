{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Lifx.Lan.LowLevel.Types
    ( MessageType(..),
      Header(..),
      dfltHdr,
      dfltHdrSize,
      Waveform(..),
      Interface,
      HSBK16 ) where

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Data.Binary
    ( Binary(..),
      putWord8,
      getWord8,
      encode )
import Data.Binary.Put ( putWord64be, putWord64le, putWord32le, putWord16le )
import Data.Binary.Get
    ( getWord64be, getWord64le, getWord32le, getWord16le, skip )
import Data.Bits ( Bits(shiftL, testBit) )
import qualified Data.ByteString.Lazy as L ( length )
import qualified Data.Text as T
import Data.Word ( Word8, Word16, Word32, Word64 )

import Lifx
import Lifx.Lan.LowLevel.Util

type Interface = T.Text
type HSBK16 = HSBK Word16

instance Binary HSBK16 where
  put x = do
    putWord16le $ hue x
    putWord16le $ saturation x
    putWord16le $ brightness x
    putWord16le $ kelvin x

  get = HSBK <$> getWord16le <*> getWord16le <*> getWord16le <*> getWord16le


data Waveform = Saw | Sine | HalfSine | Triangle | Pulse
              deriving (Eq, Ord, Show, Read, Bounded, Enum)


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
    , hdrTarget      :: DeviceId
    -- 16 bits of padding for hdrTarget
    -- Reserved48
    -- Reserved6
    , hdrAckRequired :: !Bool
    , hdrResRequired :: !Bool
    , hdrSequence    :: !Word8
    -- Reserved64
    , hdrType        :: !Word16
    -- Reserved16
    } deriving (Show, Read, Eq, Ord)

all0dev :: DeviceId
all0dev = read "000000000000"

dfltHdr = dh { hdrSize = fromIntegral $ L.length $ encode dh }
  where dh = Header { hdrSize = 0
                    , hdrOrigin = 0
                    , hdrTagged = False
                    , hdrAddressable = True
                    , hdrProtocol = 1024
                    , hdrSource = 91376
                    , hdrTarget = all0dev
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
    put $ hdrTarget h
    putWord64le 0 -- 16 bits padding + Reserved48
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
              <*> get         -- hdrTarget
    skip 8 -- 16 bits padding + Reserved48
    ar <- getWord8
    let hhhh = hhh (testBit ar bAckRequired) (testBit ar bResRequired)
    hhhhh <- hhhh <$> getWord8 -- hdrSequence
    skip 8 -- Reserved64
    hhhhhh <- hhhhh <$> getWord16le -- hdrType
    skip 2 -- Reserved16
    return hhhhhh

class MessageType t where
  msgType :: t -> Word16

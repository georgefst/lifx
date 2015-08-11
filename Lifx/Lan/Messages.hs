module Lifx.Lan.Messages
    ( SetWaveform(..),
      StateInfo(..),
      StateVersion(..),
      StateWifiFirmware(..),
      StateHostFirmware(..),
      getHostInfo,
      getHostFirmware,
      getWifiFirmware,
      setPower,
      getVersion,
      getInfo,
      getLight,
      setColor,
      setWaveform ) where

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Control.Concurrent.STM ( atomically )
import Data.Binary
    ( Binary(..), putWord8, getWord8 )
import Data.Binary.Put
    ( putWord64le, putWord32le, putWord16le, putLazyByteString )
import Data.Binary.Get
    ( skip, getWord64le, getWord32le, getWord16le, getLazyByteString )
import qualified Data.ByteString.Lazy as L ( ByteString, takeWhile )
import Data.Int ( Int16 )
import Data.Word ( Word16, Word32, Word64 )

import Lifx.Lan.Util
import Lifx.Lan.Types
import Lifx.Lan.Protocol

labelSize = 32

ackCb :: IO () -> Header -> Acknowledgement -> IO ()
ackCb cb hdr ack = cb

needAck :: Header -> Header
needAck hdr = hdr { hdrAckRequired = True }

----------------------------------------------------------

-- (GetService and StateService are in Protocol.hs)

----------------------------------------------------------

data GetHostInfo = GetHostInfo

instance MessageType GetHostInfo where
  msgType _ = 12

instance Binary GetHostInfo where
  put _ = return ()
  get = return GetHostInfo

getHostInfo :: Bulb -> (StateHostInfo -> IO ()) -> IO ()
getHostInfo bulb@(Bulb st _ _) cb = do
  hdr <- atomically $ newHdrAndCallback st (const cb)
  sendMsg bulb hdr GetHostInfo

----------------------------------------------------------

data StateHostInfo
  = StateHostInfo
    { shiSignal :: !Float
    , shiTX :: !Word32
    , shiRX :: !Word32
    , shiMcuTemperature :: !Int16 -- in hundredths of a degree Celsius
    } deriving Show

instance MessageType StateHostInfo where
  msgType _ = 13

instance Binary StateHostInfo where
  put x = do
    putFloat32le $ shiSignal x
    putWord32le $ shiTX x
    putWord32le $ shiRX x
    putInt16le $ shiMcuTemperature x

  get =
    StateHostInfo <$> getFloat32le <*> getWord32le <*> getWord32le <*> getInt16le

----------------------------------------------------------

data GetHostFirmware = GetHostFirmware

instance MessageType GetHostFirmware where
  msgType _ = 14

instance Binary GetHostFirmware where
  put _ = return ()
  get = return GetHostFirmware

getHostFirmware :: Bulb -> (StateHostFirmware -> IO ()) -> IO ()
getHostFirmware bulb@(Bulb st _ _) cb = do
  hdr <- atomically $ newHdrAndCallback st (const cb)
  sendMsg bulb hdr GetHostFirmware

----------------------------------------------------------

data StateHostFirmware
  = StateHostFirmware
    { shfBuild :: !Word64
      -- Reserved64
    , shfVersion :: !Word32
    } deriving Show

instance MessageType StateHostFirmware where
  msgType _ = 15

instance Binary StateHostFirmware where
  put x = do
    putWord64le $ shfBuild x
    putWord64le 0 -- Reserved64
    putWord32le $ shfVersion x

  get = do
    build <- getWord64le
    skip 8 -- Reserved64
    version <- getWord32le
    return $ StateHostFirmware build version

----------------------------------------------------------

data GetWifiFirmware = GetWifiFirmware

instance MessageType GetWifiFirmware where
  msgType _ = 18

instance Binary GetWifiFirmware where
  put _ = return ()
  get = return GetWifiFirmware

getWifiFirmware :: Bulb -> (StateWifiFirmware -> IO ()) -> IO ()
getWifiFirmware bulb@(Bulb st _ _) cb = do
  hdr <- atomically $ newHdrAndCallback st (const cb)
  sendMsg bulb hdr GetWifiFirmware

----------------------------------------------------------

data StateWifiFirmware
  = StateWifiFirmware
    { swfBuild :: !Word64
      -- Reserved64
    , swfVersion :: !Word32
    } deriving Show

instance MessageType StateWifiFirmware where
  msgType _ = 19

instance Binary StateWifiFirmware where
  put x = do
    putWord64le $ swfBuild x
    putWord64le 0 -- Reserved64
    putWord32le $ swfVersion x

  get = do
    build <- getWord64le
    skip 8 -- Reserved64
    version <- getWord32le
    return $ StateWifiFirmware build version

----------------------------------------------------------

data SetPower = SetPower { spLevel :: !Word16 }

instance MessageType SetPower where
  msgType _ = 21

instance Binary SetPower where
  put x = putWord16le $ spLevel x

  get = SetPower <$> getWord16le

setPower :: Bulb -> Bool -> IO () -> IO ()
setPower bulb@(Bulb st _ _) pwr cb = do
  hdr <- atomically $ newHdrAndCallback st (ackCb cb)
  sendMsg bulb (needAck hdr) (SetPower $ f pwr)
  where f True = 0xffff
        f False = 0

----------------------------------------------------------

data GetVersion = GetVersion

instance MessageType GetVersion where
  msgType _ = 32

instance Binary GetVersion where
  put _ = return ()
  get = return GetVersion

getVersion :: Bulb -> (StateVersion -> IO ()) -> IO ()
getVersion bulb@(Bulb st _ _) cb = do
  hdr <- atomically $ newHdrAndCallback st (const cb)
  sendMsg bulb hdr GetVersion

----------------------------------------------------------

data StateVersion =
  StateVersion
  { svVendor :: !Word32
  , svProduct :: !Word32
  , svVersion :: !Word32
  } deriving Show

instance MessageType StateVersion where
  msgType _ = 33

instance Binary StateVersion where
  put x = do
    putWord32le $ svVendor x
    putWord32le $ svProduct x
    putWord32le $ svVersion x

  get = StateVersion <$> getWord32le <*> getWord32le <*> getWord32le

----------------------------------------------------------

data GetInfo = GetInfo

instance MessageType GetInfo where
  msgType _ = 34

instance Binary GetInfo where
  put _ = return ()
  get = return GetInfo

getInfo :: Bulb -> (StateInfo -> IO ()) -> IO ()
getInfo bulb@(Bulb st _ _) cb = do
  hdr <- atomically $ newHdrAndCallback st (const cb)
  sendMsg bulb hdr GetInfo

----------------------------------------------------------

data StateInfo =
  StateInfo
  { siTime :: !Word64
  , siUptime :: !Word64
  , siDowntime :: !Word64
  } deriving Show

instance MessageType StateInfo where
  msgType _ = 35

instance Binary StateInfo where
  put x = do
    putWord64le $ siTime x
    putWord64le $ siUptime x
    putWord64le $ siDowntime x

  get = StateInfo <$> getWord64le <*> getWord64le <*> getWord64le

----------------------------------------------------------

data Acknowledgement = Acknowledgement

instance MessageType Acknowledgement where
  msgType _ = 45

instance Binary Acknowledgement where
  put _ = return ()
  get = return Acknowledgement

----------------------------------------------------------

data GetLight = GetLight

instance MessageType GetLight where
  msgType _ = 101

instance Binary GetLight where
  put _ = return ()
  get = return GetLight

getLight :: Bulb -> (StateLight -> IO ()) -> IO ()
getLight bulb @(Bulb st _ _) cb = do
  hdr <- atomically $ newHdrAndCallback st (const cb)
  sendMsg bulb hdr GetLight

----------------------------------------------------------

data StateLight =
  StateLight
  { slColor :: HSBK
    -- Reserved16 (dim)
  , slPower :: !Word16
  , slLabel :: L.ByteString -- 32 bytes (aka labelSize)
    -- Reserved64 (tags)
  } deriving Show

instance MessageType StateLight where
  msgType _ = 107

instance Binary StateLight where
  put x = do
    put $ slColor x
    putWord16le 0 -- Reserved16 (dim)
    putWord16le $ slPower x
    putLazyByteString $ padByteString labelSize $ slLabel x
    putWord64le 0 -- Reserved64 (tags)

  get = do
    color <- get
    skip 2 -- Reserved16 (dim)
    power <- getWord16le
    label <- getLazyByteString labelSize
    skip 8 -- Reserved64 (tags)
    return $ StateLight color power $ L.takeWhile (/= 0) label

----------------------------------------------------------

data SetColor =
  SetColor
  { -- Reserved8 (stream)
    scColor :: HSBK
  , scDuration :: !Word32
  }

instance MessageType SetColor where
  msgType _ = 102

instance Binary SetColor where
  put x = do
    putWord8 0 -- Reserved8 (stream)
    put $ scColor x
    putWord32le $ scDuration x

  get = do
    skip 1 -- Reserved8 (stream)
    SetColor <$> get <*> getWord32le

setColor :: Bulb -> HSBK -> Word32 -> IO () -> IO ()
setColor bulb@(Bulb st _ _) color duration cb = do
  hdr <- atomically $ newHdrAndCallback st (ackCb cb)
  sendMsg bulb (needAck hdr) (SetColor color duration)

----------------------------------------------------------

data SetWaveform =
  SetWaveform
  { -- Reserved8 (stream)
    swTransient :: !Bool
  , swColor :: HSBK
  , swPeriod :: !Word32
  , swCycles :: !Float
  , swDutyCycle :: !Int16
  , swWaveform :: Waveform
  } deriving Show

instance MessageType SetWaveform where
  msgType _ = 103

instance Binary SetWaveform where
  put x = do
    putWord8 0 -- Reserved8 (stream)
    putWord8 (if swTransient x then 1 else 0)
    put $ swColor x
    putWord32le $ swPeriod x
    putFloat32le $ swCycles x
    putInt16le $ swDutyCycle x
    putWord8 $ fromIntegral $ fromEnum $ swWaveform x

  get = do
    skip 1 -- Reserved8 (stream)
    t <- getWord8
    let trans = (t /= 0)
    x <- (SetWaveform trans) <$> get <*> getWord32le
         <*> getFloat32le <*> getInt16le
    w <- getWord8
    return (x $ toEnum $ fromIntegral w)

setWaveform :: Bulb -> SetWaveform -> IO () -> IO ()
setWaveform bulb@(Bulb st _ _) swf cb = do
  hdr <- atomically $ newHdrAndCallback st (ackCb cb)
  sendMsg bulb (needAck hdr) swf
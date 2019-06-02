{-|
Module      : System.Hardware.Lifx.Lan
Description : Implementation of Connection for LIFX Lan Protocol
Copyright   : © Patrick Pelletier, 2016
License     : BSD3
Maintainer  : code@funwithsoftware.org
Stability   : experimental
Portability : GHC

This module implements a 'Connection' for controlling LIFX bulbs
via the LIFX Lan Protocol.
-}

{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.Lifx.Lan
    ( LanSettings(..)
    , LanConnection
    , defaultLanSettings
    , openLanConnection
    , newConnectionGG
    , getLan
    , clBulb
    , lcLights
    ) where

import System.Hardware.Lifx
import System.Hardware.Lifx.Connection
import qualified System.Hardware.Lifx.Connection as E( EffectType( Pulse ) )
import System.Hardware.Lifx.Internal
import System.Hardware.Lifx.Lan.LowLevel hiding (setLabel)
import qualified System.Hardware.Lifx.Lan.LowLevel as W( Waveform( Pulse ) , setLabel )
import System.Hardware.Lifx.Lan.LowLevel.Internal (untilKilled, endThread)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Hourglass
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version
import Data.Word
import GHC.Float
import System.IO
import System.Mem.Weak
import Time.System

-- | Parameters which can be passed to 'openLanConnection'.
data LanSettings =
  LanSettings
  { -- | 'IO' action which returns name of network interface to use,
    -- such as @en1@ or @eth0@.  The default action is to look in
    -- @~\/.config\/hs-lifx\/config.json@.  If @interface@ is not specified
    -- in the config file, the default is 'Nothing', which lets the
    -- operating system choose the interface.
    lsIfName      :: IO (Maybe Interface)
    -- | Function to log a line of text.  This contains
    -- information which might be helpful for troubleshooting.
    -- Default is 'TIO.hPutStrLn' 'stderr'.
  , lsLog         :: T.Text -> IO ()
    -- | Unlike the Cloud API, the LAN Protocol does not have a notion of
    -- scenes built-in.  Therefore, you can provide this function to
    -- implement scenes for the LAN Protocol.  It should simply return a
    -- list of all the scenes.  Default is to return an empty list.
  , lsListScenes  :: IO [Scene]
    -- | Specifies timeouts and how aggressively to retry when messages
    -- time out.  Default is 'defaultRetryParams'.
  , lsRetryParams :: RetryParams
    -- | How frequently to poll the network for new devices.
    -- Default is 1.5 seconds.
  , lsDiscoveryPollInterval :: FracSeconds
    -- | If a bulb is not seen for this amount of time, it is marked Offline.
    -- Default is 5 seconds.
  , lsOfflineInterval :: FracSeconds
    -- | Port that LIFX bulbs are listening on.  Default is @56700@, which
    -- is the correct value for LIFX bulbs.  The only reason to change this
    -- is if you want to mock the bulbs for testing.
  , lsPort        :: !Word16
  }

-- | Returns of 'LanSettings' with default settings.
defaultLanSettings :: LanSettings
defaultLanSettings =
  LanSettings
  { lsIfName      = return Nothing
  , lsLog         = TIO.hPutStrLn stderr
  , lsPort        = 56700
  , lsListScenes  = return []
  , lsRetryParams = defaultRetryParams
  , lsDiscoveryPollInterval = 1.5
  , lsOfflineInterval = 5
  }

data CachedThing a = NotCached | Cached DateTime a deriving (Show, Eq, Ord)

data CachedLight =
  CachedLight
  { clBulb     :: Bulb
  , clLocation :: CachedThing LocationId
  , clGroup    :: CachedThing GroupId
  , clLabel    :: CachedThing Label
  , clFirstSeen :: DateTime
  } deriving (Show, Eq, Ord)

data CachedLabel =
  CachedLabel
  { claLabel     :: Label
  , claUpdatedAt :: !Word64
  } deriving (Show, Eq, Ord)

-- | Opaque type which implements 'Connection' and represents a connection
-- to all LIFX devices on a LAN.  It's OK to use a @LanConnection@ from
-- multiple threads at once.
data LanConnection =
  LanConnection
  { lcLan       :: Lan
  , lcSettings  :: LanSettings
  , lcLights    :: TVar (M.Map DeviceId   CachedLight)
  , lcGroups    :: TVar (M.Map GroupId    CachedLabel)
  , lcLocations :: TVar (M.Map LocationId CachedLabel)
  , lcThread    :: Weak ThreadId
  }

instance Show LanConnection where
  show (LanConnection { lcLan = lan }) = show lan

instance Eq LanConnection where
  x1 == x2 = x1 `compare` x2 == EQ

instance Ord LanConnection where
  (LanConnection { lcLan = lan1 }) `compare` (LanConnection { lcLan = lan2 }) =
    lan1 `compare` lan2

tvEmptyMap :: IO (TVar (M.Map a b))
tvEmptyMap = newTVarIO M.empty

-- | Create a new 'LanConnection', based on 'LanSettings'.
openLanConnection :: LanSettings -> IO LanConnection
openLanConnection ls = do
  iface <- lsIfName ls
  lan <- openLan' iface (Just $ lsPort ls) (Just $ lsLog ls)
  m1 <- tvEmptyMap
  m2 <- tvEmptyMap
  m3 <- tvEmptyMap
  tmv <- newEmptyTMVarIO
  thr <- forkFinally (discoveryThread tmv) (\_ -> closeLan lan)
  wthr <- mkWeakThreadId thr
  atomically $ do
    let lc = LanConnection lan ls m1 m2 m3 wthr
    putTMVar tmv lc
    return lc

-- | Returns the underlying low-level 'Lan' that this 'LanConnection' uses.
-- This is useful if you want to break out of the high-level abstraction
-- provided by 'LanConnection' and do something low-level.
getLan :: LanConnection -> Lan
getLan = lcLan


listOfMVarToList :: [MVar (Either SomeException a)] -> IO [a]
listOfMVarToList = mapM takeMVarThrow

takeMVarThrow :: MVar (Either SomeException a) -> IO a
takeMVarThrow mv = do
  v <- takeMVar mv
  case v of
   Left exc -> throwIO exc
   Right r -> return r

forkFinallyMVar :: (MVar (Either SomeException a)) -> IO a -> IO ThreadId
forkFinallyMVar mv f = forkFinally f (putMVar mv)

emptyLightInfo :: DeviceId -> DateTime -> LightInfo
emptyLightInfo devId now = LightInfo
  { lId = devId
  , lUuid = Nothing
  , lLabel = Nothing
  , lConnected = False
  , lPower = Nothing
  , lColor = emptyColor
  , lGroupId = Nothing
  , lGroup = Nothing
  , lLocationId = Nothing
  , lLocation = Nothing
  , lLastSeen = now
  , lSecondsSinceSeen = 0
  , lProduct = Nothing
  , lTemperature = Nothing
  , lUptime = Nothing
  , lFirmwareVersion = Nothing
  , lHardwareVersion = Nothing
  }


data MessageNeeded = NeedGetLight   | NeedGetGroup    | NeedGetLocation
                   | NeedGetVersion | NeedGetHostInfo | NeedGetInfo
                   | NeedGetHostFirmware
                   deriving (Show, Read, Eq, Ord, Bounded, Enum)

needMessage :: InfoNeeded -> MessageNeeded
needMessage NeedLabel           = NeedGetLight
needMessage NeedPower           = NeedGetLight
needMessage NeedColor           = NeedGetLight
needMessage NeedGroup           = NeedGetGroup
needMessage NeedLocation        = NeedGetLocation
needMessage NeedProduct         = NeedGetVersion
needMessage NeedTemperature     = NeedGetHostInfo
needMessage NeedUptime          = NeedGetInfo
needMessage NeedFirmwareVersion = NeedGetHostFirmware
needMessage NeedHardwareVersion = NeedGetVersion


whatsNeeded :: [InfoNeeded] -> [MessageNeeded]
whatsNeeded needed = sort $ nub $ map needMessage needed


type FinCont = LightInfo -> IO ()
type NxtCont = LightInfo -> IO ()

color16toFrac :: HSBK16 -> Color
color16toFrac c = HSBK
  { hue = fromIntegral (hue c) / 65535 * 360
  , saturation = fromIntegral (saturation c) / 65535
  , brightness = fromIntegral (brightness c) / 65535
  , kelvin = fromIntegral (kelvin c)
  }

colorFracTo16 :: Color -> HSBK16
colorFracTo16 c = HSBK
  { hue = round $ hue c * 65535 / 360
  , saturation = round $ saturation c * 65535
  , brightness = round $ brightness c * 65535
  , kelvin = round $ kelvin c
  }

justColor :: Color -> PartialColor
justColor = fmap Just

definitelyColor :: PartialColor -> Color
definitelyColor = fmap fromJust

color16ToMaybeFrac :: HSBK16 -> PartialColor
color16ToMaybeFrac hsbk = justColor $ color16toFrac hsbk

maxDuration :: FracSeconds
maxDuration =
  let maxms = maxBound :: Word32
  in (fromIntegral maxms) / 1000

f2ms :: FracSeconds -> Word32
f2ms x = round $ 1000 * x

f2µs :: FracSeconds -> Int
f2µs x = round $ 1e6 * x

nanosPerSecond :: FracSeconds
nanosPerSecond = 1e9

checkDuration :: T.Text -> FracSeconds -> IO ()
checkDuration = checkParam 0 maxDuration

checkParam :: FracSeconds -> FracSeconds -> T.Text -> FracSeconds -> IO ()
checkParam mn mx name param =
  when (param < mn || param > mx) $ throwIO $ BadParam $ InvalidRange name mn mx

unpackFirmwareVersion :: Word32 -> Version
unpackFirmwareVersion v = Version (map fromIntegral [major, minor]) []
  where major = v `shiftR` 16
        minor = v .&. 0xffff

lastSeen :: CachedLight -> DateTime
lastSeen cl = fromMaybe (clFirstSeen cl) $
              maximum [ dtOfCt (clLocation cl)
                      , dtOfCt (clGroup    cl)
                      , dtOfCt (clLabel    cl)
                      ]

timeDiffFracSeconds :: (Timeable t1, Timeable t2) => t1 -> t2 -> FracSeconds
timeDiffFracSeconds t1 t2 =
  let (Seconds s, NanoSeconds ns) = timeDiffP t1 t2
      [s', ns'] = map fromIntegral [s, ns]
  in s' + ns' / 1e9

data FilterResult = Accept | Reject | Unknown

isAccept :: FilterResult -> Bool
isAccept Accept = True
isAccept _ = False

filterLights :: TVar (M.Map DeviceId CachedLight)
                -> (CachedLight -> FilterResult)
                -> STM [CachedLight]
filterLights tv f = do
  m <- readTVar tv
  let lites = M.elems m
  return $ filter (isAccept . f) lites

findLabel :: TVar (M.Map a CachedLabel) -> Label -> STM (Maybe a)
findLabel tv lbl = do
  m <- readTVar tv
  let lst = map f $ M.toList m
      f (x, y) = (claLabel y, x)
  return $ lookup lbl lst

b2fr :: Bool -> FilterResult
b2fr True = Accept
b2fr False = Reject


checkAlive :: DateTime
              -> FracSeconds
              -> (CachedLight -> IO (MVar (Either SomeException a)))
              -> (CachedLight -> a)
              -> CachedLight
              -> IO (MVar (Either SomeException a))
checkAlive now offlineInterval ifAlive ifDead lite =
  if (now `timeDiffFracSeconds` lastSeen lite <= offlineInterval)
  then ifAlive lite
  else do
    mv <- newEmptyMVar
    putMVar mv $ Right $ ifDead lite
    return mv


offlineResult :: CachedLight -> Result
offlineResult lite = Result
  { rId = deviceId $ clBulb lite
  , rLabel = maybeFromCached $ clLabel lite
  , rStatus = Offline
  }

offlineLightInfo :: DateTime
                 -> M.Map GroupId CachedLabel
                 -> M.Map LocationId CachedLabel
                 -> CachedLight
                 -> LightInfo
offlineLightInfo now groups locations lite =
  (emptyLightInfo (deviceId $ clBulb lite) last)
    { lLabel = maybeFromCached $ clLabel lite
    , lConnected = False
    , lGroupId = grp
    , lGroup = maybeLookup grp groups
    , lLocationId = loc
    , lLocation = maybeLookup loc locations
    , lSecondsSinceSeen = since
    }
  where last = lastSeen lite
        since = now `timeDiffFracSeconds` last
        grp = maybeFromCached $ clGroup lite
        loc = maybeFromCached $ clLocation lite
        maybeLookup Nothing _ = Nothing
        maybeLookup (Just k) m = fmap claLabel $ M.lookup k m

dtOfCt :: CachedThing a -> Maybe DateTime
dtOfCt NotCached = Nothing
dtOfCt (Cached dt _ ) = Just dt

microsPerSecond = 1e6
-- discoveryTime = 1.5
fastDiscoveryTime = 0.25

discoveryThread :: TMVar LanConnection -> IO ()
discoveryThread tmv = do
  lc <- atomically $ takeTMVar tmv
  let discoveryTime = lsDiscoveryPollInterval $ lcSettings lc
  forM_ [1..3] $ \_ -> do
    db lc
    td $ min discoveryTime fastDiscoveryTime
  untilKilled (lsLog $ lcSettings lc) "discovery" $ do
    db lc
    td discoveryTime
  where
    db lc = discoverBulbs (lcLan lc) $ discoveryCb lc
    td secs = threadDelay $ floor $ microsPerSecond * secs

data Query = QueryLocation | QueryGroup | QueryLabel deriving (Show, Eq, Ord)

discoveryCb :: LanConnection -> Bulb -> IO ()
discoveryCb lc bulb = do
  now <- dateCurrent
  queries <- atomically $ do
    lites <- readTVar (lcLights lc)
    case deviceId bulb `M.lookup` lites of
     Nothing -> do
       let lite = CachedLight bulb NotCached NotCached NotCached now
       writeTVar (lcLights lc) $ M.insert (deviceId bulb) lite lites
       return [ QueryLocation , QueryGroup , QueryLabel ] -- update all three
     (Just lite) -> do
       -- in case bulb changes (same id, new ip)
       when (bulb /= clBulb lite) $
         let lite' = lite { clBulb = bulb }
         in writeTVar (lcLights lc) $ M.insert (deviceId bulb) lite' lites
       let pairs = [ (dtOfCt (clLocation lite), QueryLocation)
                   , (dtOfCt (clGroup    lite), QueryGroup)
                   , (dtOfCt (clLabel    lite), QueryLabel) ]
       return [ snd $ minimum pairs ] -- just update the oldest one
  doQuery queries

  where
    -- FIXME: need to do reliable query?  or just accept lossiness?
    doQuery [] = return ()
    doQuery (QueryLocation:qs) = getLocation bulb $ \slo -> do
      now <- dateCurrent
      atomically $ updateLocation lc dev now slo
      doQuery qs
    doQuery (QueryGroup:qs) = getGroup bulb $ \sg -> do
      now <- dateCurrent
      atomically $ updateGroup lc dev now sg
      doQuery qs
    doQuery (QueryLabel:qs) = getLight bulb $ \sl -> do
      now <- dateCurrent
      atomically $ updateLabel lc dev now sl
      doQuery qs

    dev = deviceId bulb

updateCachedLight :: LanConnection -> DeviceId -> (CachedLight -> CachedLight)
                     -> STM ()
updateCachedLight lc dev f = do
  lites <- readTVar (lcLights lc)
  let lites' = M.adjust f dev lites
  writeTVar (lcLights lc) lites'

updateCachedLabel :: Ord a
                     => TVar (M.Map a CachedLabel)
                     -> a
                     -> Label
                     -> Word64
                     -> STM ()
updateCachedLabel tv k lbl updatedAt = do
  cache <- readTVar tv
  let needUpd =
        case k `M.lookup` cache of
         Nothing -> True
         (Just (CachedLabel { claUpdatedAt = upat })) -> upat < updatedAt
      cl = CachedLabel { claLabel = lbl , claUpdatedAt = updatedAt }
  when needUpd $ writeTVar tv $ M.insert k cl cache

updateLocation :: LanConnection -> DeviceId -> DateTime -> StateLocation
                  -> STM ()
updateLocation lc dev now slo = do
  let lid = sloLocation slo
  updateCachedLight lc dev $ \cl -> cl { clLocation = Cached now lid }
  updateCachedLabel (lcLocations lc) lid (sloLabel slo) (sloUpdatedAt slo)

updateGroup :: LanConnection -> DeviceId -> DateTime -> StateGroup
               -> STM ()
updateGroup lc dev now sg = do
  let gid = sgGroup sg
  updateCachedLight lc dev $ \cl -> cl { clGroup = Cached now gid }
  updateCachedLabel (lcGroups lc) gid (sgLabel sg) (sgUpdatedAt sg)

updateLabel :: LanConnection -> DeviceId -> DateTime -> StateLight
               -> STM ()
updateLabel lc dev now sl =
  updateCachedLight lc dev $ \cl -> cl { clLabel = Cached now (slLabel sl) }

-- If Kelvin is set, set saturation to 0 (white), unless saturation is also
-- explicitly set.
desaturateKelvin :: PartialColor -> PartialColor
desaturateKelvin (HSBK h Nothing b k@(Just _ )) = HSBK h (Just 0) b k
desaturateKelvin x = x

maybeFromCached :: CachedThing a -> Maybe a
maybeFromCached NotCached = Nothing
maybeFromCached (Cached _ x) = Just x


effectTypeToWaveform :: EffectType -> Waveform
effectTypeToWaveform E.Pulse = W.Pulse
effectTypeToWaveform Breathe = Sine



checkEffect :: Effect -> IO ()
checkEffect eff = do
  checkDuration "period" $ ePeriod eff
  checkParam 0 1 "peak" $ ePeak eff

checkTransition :: StateTransition -> IO ()
checkTransition st = do
  checkDuration "duration" $ sDuration st
  checkColor $ sColor st

checkColor :: PartialColor -> IO ()
checkColor c = do
  checkComponent 0 360 "hue" $ hue c
  checkComponent 0 1 "saturation" $ saturation c
  checkComponent 0 1 "brightness" $ brightness c
  checkComponent minKelvin maxKelvin "kelvin" $ kelvin c

checkComponent :: ColorChannel -> ColorChannel -> T.Text -> Maybe ColorChannel -> IO ()
checkComponent _ _ _ Nothing = return ()
checkComponent mn mx name (Just x) = checkParam mn mx name x

newConnectionGG :: IO LanConnection
newConnectionGG = do
    con <- openLanConnection defaultLanSettings
    threadDelay 1000000
    return con

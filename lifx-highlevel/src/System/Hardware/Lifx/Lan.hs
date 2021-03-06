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
    , UnknownSelectorBehavior(..)
    , defaultLanSettings
    , openLanConnection
    , getLan
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
    -- | Specifies whether an unknown selector should result in an
    -- exception (which matches the cloud behavior), or should just be
    -- ignored.  Default is 'ThrowOnUnknownSelector'.
  , lsUnknownSelectorBehavior :: UnknownSelectorBehavior
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
  { lsIfName      = cfgInterface <$> getConfig
  , lsLog         = TIO.hPutStrLn stderr
  , lsPort        = 56700
  , lsListScenes  = return []
  , lsRetryParams = defaultRetryParams
  , lsDiscoveryPollInterval = 1.5
  , lsOfflineInterval = 5
  , lsUnknownSelectorBehavior = ThrowOnUnknownSelector
  }

-- | The two possible ways to handle a 'Selector' which does not match a
-- device ID, group, etc. which is currently on the LAN.
data UnknownSelectorBehavior =
    -- Removes any unknown selectors from the list of selectors, and then
    -- proceeds as if only the known selectors (if any) had been specified.
    IgnoreUnknownSelector
    -- Throws 'SelectorNotFound'.  This matches the Cloud API behavior.
    -- However, the Cloud API knowns about all devices associated with an
    -- account, whether they are on or not.  A 'LanConnection' only knows
    -- about devices which it has seen since the connection was established.
    -- For this reason, throwing an exception for unknown selectors may
    -- not be as desirable on a 'LanConnection' as it is on a
    -- @CloudConnection@.
  | ThrowOnUnknownSelector
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

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

listOneLight :: LanConnection
                -> [MessageNeeded]
                -> CachedLight
                -> IO (MVar (Either SomeException LightInfo))
listOneLight lc messagesNeeded cl = do
  mv <- newEmptyMVar
  let fin li = do
        now <- dateCurrent
        putMVar mv $ Right $ adjustSeen now li
  gatherInfo (lc, bulb, fin) messagesNeeded eli
  return mv

  where bulb = clBulb cl
        eli = emptyLightInfo (deviceId bulb) (lastSeen cl)

        gatherInfo (_ , _ , fin) [] li = fin $ li { lConnected = True }
        gatherInfo stuff (mneed:mneeds) li =
          cbForMessage stuff mneed (gatherInfo stuff mneeds) li

        adjustSeen now li =
          let secs = timeDiffFracSeconds now (lLastSeen li)
          in li { lSecondsSinceSeen = secs }

timeDiffFracSeconds :: (Timeable t1, Timeable t2) => t1 -> t2 -> FracSeconds
timeDiffFracSeconds t1 t2 =
  let (Seconds s, NanoSeconds ns) = timeDiffP t1 t2
      [s', ns'] = map fromIntegral [s, ns]
  in s' + ns' / 1e9

cbForMessage :: (LanConnection, Bulb, FinCont)
                -> MessageNeeded
                -> NxtCont
                -> LightInfo
                -> IO ()
cbForMessage (lc, bulb, finCont) mneed nxtCont li = f mneed
  where ls = lcSettings lc
        rq q cb = reliableQuery (lsRetryParams ls) (q bulb) cb $ do
                    (lsLog ls)
                      (T.pack $ show bulb ++ " not responding to " ++ opName)
                    finCont li
        opName = drop 4 $ show mneed

        f NeedGetLight        = rq getLight        cbLight
        f NeedGetGroup        = rq getGroup        cbGroup
        f NeedGetLocation     = rq getLocation     cbLocation
        f NeedGetVersion      = rq getVersion      cbVersion
        f NeedGetHostInfo     = rq getHostInfo     cbHostInfo
        f NeedGetInfo         = rq getInfo         cbInfo
        f NeedGetHostFirmware = rq getHostFirmware cbHostFirmware

        cbLight sl         = do
          now <- dateCurrent
          atomically $ updateLabel lc (deviceId bulb) now sl
          nxtCont (trLight sl)
        cbGroup sg         = do
          now <- dateCurrent
          groups <- atomically $ do
            updateGroup lc (deviceId bulb) now sg
            readTVar (lcGroups lc)
          nxtCont (trGroup sg groups)
        cbLocation slo     = do
          now <- dateCurrent
          locations <- atomically $ do
            updateLocation lc (deviceId bulb) now slo
            readTVar (lcLocations lc)
          nxtCont (trLocation slo locations)
        cbVersion sv       = nxtCont (trVersion sv)
        cbHostInfo shi     = nxtCont (trHostInfo shi)
        cbInfo si          = nxtCont (trInfo si)
        cbHostFirmware shf = nxtCont (trHostFirmware shf)

        trLight sl = li { lColor = color16ToMaybeFrac (slColor sl)
                        , lPower = Just (slPower sl)
                        , lLabel = Just (slLabel sl)
                        }
        trGroup sg groups =
          li { lGroupId = Just (sgGroup sg)
             , lGroup   = claLabel <$> M.lookup (sgGroup sg) groups
             }
        trLocation slo locations =
          li { lLocationId = Just (sloLocation slo)
             , lLocation   = claLabel <$> M.lookup (sloLocation slo) locations
             }
        trVersion sv =
          li { lProduct = productFromId (svVendor sv) (svProduct sv)
             , lHardwareVersion = Just (fromIntegral $ svVersion sv)
             }
        trHostInfo shi =
          li { lTemperature = Just (fromIntegral (shiMcuTemperature shi) / 100) }
        trInfo si =
          li { lUptime = Just (fromIntegral (siUptime si) / nanosPerSecond) }
        trHostFirmware shf =
          li { lFirmwareVersion = Just (unpackFirmwareVersion $ shfVersion shf) }

behave4US :: UnknownSelectorBehavior -> Selector -> IO [a]
behave4US IgnoreUnknownSelector  _   = return []
behave4US ThrowOnUnknownSelector sel = throwIO $ SelectorNotFound sel

behave4USLC :: LanConnection -> Selector -> IO [a]
behave4USLC lc = behave4US (lsUnknownSelectorBehavior $ lcSettings lc)

applySelectors :: LanConnection -> [Selector] -> IO [CachedLight]
applySelectors lc sels = do
  sels' <- case find isScene sels of
            Nothing -> return sels
            Just _ -> do
              scenes <- lsListScenes $ lcSettings lc
              expanded <- mapM (expandScene scenes) sels
              return $ concat expanded
  applySelectors' lc sels'

  where isScene (SelSceneId _ ) = True
        isScene _ = False

        expandScene scenes sel@(SelSceneId sid) = do
          case find (\s -> sid == scId s) scenes of
           Nothing -> behave4USLC lc sel
           (Just scene) -> mapM (avoidNested sid . ssSel) (scStates scene)
        expandScene _ sel = return [sel]

        avoidNested parentSid (SelSceneId sid) =
          throwIO $ NestedSceneIdSelector parentSid sid
        avoidNested _ sel = return sel

applySelectors' :: LanConnection -> [Selector] -> IO [CachedLight]
applySelectors' lc sels = do
  lists <- atomically $ mapM (selectLights lc) sels
  let pairs = zip sels lists
      behave (sel, [])   = behave4USLC lc sel
      behave (_ , lites) = return lites
  lists' <- mapM behave pairs
  let sets = map S.fromList lists'
      uniq = S.unions sets
  return $ S.toList uniq

selectLights :: LanConnection -> Selector -> STM [CachedLight]
selectLights lc (SelGroup lbl) = do
  mby <- findLabel (lcGroups lc) lbl
  case mby of
   Nothing -> return []
   (Just gid) -> selectLights lc (SelGroupId gid)
selectLights lc (SelLocation lbl) = do
  mby <- findLabel (lcLocations lc) lbl
  case mby of
   Nothing -> return []
   (Just lid) -> selectLights lc (SelLocationId lid)
selectLights lc sel = filterLights (lcLights lc) (selectFilter sel)

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

selectFilter :: Selector -> CachedLight -> FilterResult
selectFilter SelAll _ = Accept
selectFilter (SelLabel lbl) (CachedLight {clLabel = NotCached}) = Unknown
selectFilter (SelLabel lbl) (CachedLight {clLabel = (Cached _ lbl')}) =
  b2fr (lbl == lbl')
selectFilter (SelDevId did) cl = b2fr $ did == (deviceId $ clBulb cl)
selectFilter (SelGroupId gid) (CachedLight {clGroup = NotCached}) = Unknown
selectFilter (SelGroupId gid) (CachedLight {clGroup = (Cached _ gid')}) =
  b2fr (gid == gid')
selectFilter (SelLocationId lid) (CachedLight {clLocation = NotCached}) = Unknown
selectFilter (SelLocationId lid) (CachedLight {clLocation = (Cached _ lid')}) =
  b2fr (lid == lid')
selectFilter sel _ = error $ "unimplemented selector " ++ show sel


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


doListLights :: LanConnection
                -> [Selector]
                -> [InfoNeeded]
                -> IO [MVar (Either SomeException LightInfo)]
doListLights lc sels needed = do
  let messagesNeeded = whatsNeeded needed
      oi = lsOfflineInterval $ lcSettings lc
  (groups, locations) <-
    atomically ((,) <$> readTVar (lcGroups lc) <*> readTVar (lcLocations lc))
  lites <- applySelectors lc sels
  now <- dateCurrent
  forM lites $ checkAlive now oi (listOneLight lc messagesNeeded) (offlineLightInfo now groups locations)

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

doSetStates :: LanConnection
               -> [([Selector], StateTransition)]
               -> IO [MVar (Either SomeException StateTransitionResult)]
doSetStates lc pairs = forM pairs $ setOneState lc

setOneState :: LanConnection
               -> ([Selector], StateTransition)
               -> IO (MVar (Either SomeException StateTransitionResult))
setOneState lc pair@(sels, st) = do
  mv <- newEmptyMVar
  now <- dateCurrent
  let oi = lsOfflineInterval $ lcSettings lc
  forkFinallyMVar mv $ do
    lites <- applySelectors lc sels
    results <- forM lites $ checkAlive now oi (setOneLightState lc st) offlineResult
    results' <- listOfMVarToList results
    return (StateTransitionResult pair results')
  return mv

setOneLightState :: LanConnection
                    -> StateTransition
                    -> CachedLight
                    -> IO (MVar (Either SomeException Result))
setOneLightState lc st cl = do
  mv <- newEmptyMVar
  let did = deviceId (clBulb cl)
      lbl = maybeFromCached (clLabel cl)
      putResult stat = putMVar mv (Right $ Result did lbl stat)
      timeout = putResult TimedOut
      newColor = desaturateKelvin (sColor st)
      skipGet = isEmptyColor newColor || isCompleteColor newColor
  getOneLight lc skipGet cl timeout $
    \( _ , oldColor ) -> setOneLightColor lc oldColor newColor (sDuration st) cl timeout $
      setOneLightPower lc (sPower st) (sDuration st) cl timeout $
        putResult Ok
  return mv

-- If Kelvin is set, set saturation to 0 (white), unless saturation is also
-- explicitly set.
desaturateKelvin :: PartialColor -> PartialColor
desaturateKelvin (HSBK h Nothing b k@(Just _ )) = HSBK h (Just 0) b k
desaturateKelvin x = x

maybeFromCached :: CachedThing a -> Maybe a
maybeFromCached NotCached = Nothing
maybeFromCached (Cached _ x) = Just x

getOneLight :: LanConnection
               -> Bool
               -> CachedLight
               -> IO ()
               -> ((Power, PartialColor) -> IO ())
               -> IO ()
getOneLight _ True _ _ cbSucc = cbSucc (Off, emptyColor)
getOneLight lc False cl cbFail cbSucc =
  reliableQuery rp (getLight bulb) succ cbFail
  where rp = lsRetryParams $ lcSettings lc
        bulb = clBulb cl
        succ sl = do
          now <- dateCurrent
          atomically $ updateLabel lc (deviceId bulb) now sl
          cbSucc (slPower sl, color16ToMaybeFrac $ slColor sl)

setOneLightColor :: LanConnection
                    -> PartialColor
                    -> PartialColor
                    -> FracSeconds
                    -> CachedLight
                    -> IO ()
                    -> IO ()
                    -> IO ()
setOneLightColor lc oldColor newColor dur cl cbFail cbSucc
  | isEmptyColor newColor = cbSucc
  | otherwise =
      reliableAction rp (setColor bulb c $ f2ms dur) cbSucc cbFail
  where rp = lsRetryParams $ lcSettings lc
        bulb = clBulb cl
        combinedColor = oldColor `combineColors` newColor
        c = colorFracTo16 $ definitelyColor combinedColor

setOneLightPower :: LanConnection
                    -> Maybe Power
                    -> FracSeconds
                    -> CachedLight
                    -> IO ()
                    -> IO ()
                    -> IO ()
setOneLightPower _ Nothing _ _ _ cbSucc = cbSucc
setOneLightPower lc (Just pwr) dur cl cbFail cbSucc =
  reliableAction rp (setPower bulb pwr $ f2ms dur) cbSucc cbFail
  where rp = lsRetryParams $ lcSettings lc
        bulb = clBulb cl


doEffect :: LanConnection
            -> [Selector]
            -> Effect
            -> IO [MVar (Either SomeException Result)]
doEffect lc sels eff = do
  lites <- applySelectors lc sels
  now <- dateCurrent
  let oi = lsOfflineInterval $ lcSettings lc
  forM lites $ checkAlive now oi (effectOneLight lc eff) offlineResult

effectOneLight :: LanConnection
                  -> Effect
                  -> CachedLight
                  -> IO (MVar (Either SomeException Result))
effectOneLight lc eff cl = do
  mv <- newEmptyMVar
  let did = deviceId (clBulb cl)
      lbl = maybeFromCached (clLabel cl)
      putResult stat = putMVar mv (Right $ Result did lbl stat)
      timeout = putResult TimedOut
      fromColor = eFromColor eff
      color = eColor eff
      nd2setColor = not $ isEmptyColor fromColor
      nd2combineColor = (not ((isEmptyColor fromColor) ||
                              (isCompleteColor fromColor))) ||
                        (not $ isCompleteColor color)
      nd2restoreColor = nd2setColor && not (ePersist eff)
      nd2getPwr = ePowerOn eff
      skipGet = not nd2restoreColor && not nd2getPwr && not nd2combineColor
  getOneLight lc skipGet cl timeout $
    \(origPwr, origColor) ->
      setOneLightColor lc origColor fromColor 0 cl timeout $
        let nd2ChangePwr = ePowerOn eff && (origPwr == Off)
            (newPwr, restorePwr) = if nd2ChangePwr
                                   then (Just On, Just Off)
                                   else (Nothing, Nothing)
        in setOneLightPower lc newPwr 0 cl timeout $
           setOneLightWaveform lc origColor color eff cl timeout $
           if nd2ChangePwr || nd2restoreColor
           then do
             forkIO $ do
               let dur = ePeriod eff * eCycles eff
               threadDelay $ f2µs dur
               setOneLightPower lc restorePwr 0 cl timeout $
                 setOneLightColor lc origColor origColor 0 cl timeout $
                 putResult Ok
             return ()
           else putResult Ok
  return mv

setOneLightWaveform :: LanConnection
                       -> PartialColor
                       -> PartialColor
                       -> Effect
                       -> CachedLight
                       -> IO ()
                       -> IO ()
                       -> IO ()
setOneLightWaveform lc origColor color eff cl cbFail cbSucc =
  reliableAction rp (setWaveform bulb swf) cbSucc cbFail
  where rp = lsRetryParams $ lcSettings lc
        bulb = clBulb cl
        combinedColor = origColor `combineColors` color
        swf = SetWaveform
              { swTransient = not (ePersist eff)
              , swColor = colorFracTo16 $ definitelyColor $ combinedColor
              , swPeriod = f2ms (ePeriod eff)
              , swCycles = double2Float (eCycles eff)
              , swDutyCycle = floor $ 65535 * (ePeak eff - 0.5)
              , swWaveform = effectTypeToWaveform (eType eff)
              }

effectTypeToWaveform :: EffectType -> Waveform
effectTypeToWaveform E.Pulse = W.Pulse
effectTypeToWaveform Breathe = Sine


setOneLightLabel :: LanConnection
                    -> Label
                    -> CachedLight
                    -> IO ()
                    -> IO ()
                    -> IO ()
setOneLightLabel lc lbl cl cbFail cbSucc =
  reliableAction rp (W.setLabel bulb lbl) cbSucc cbFail
  where rp = lsRetryParams $ lcSettings lc
        bulb = clBulb cl

labelOneLight :: LanConnection
                 -> Label
                 -> CachedLight
                 -> IO (MVar (Either SomeException Result))
labelOneLight lc lbl cl = do
  mv <- newEmptyMVar
  let did = deviceId (clBulb cl)
      oldLbl = maybeFromCached (clLabel cl)
      putTimeout = putMVar mv (Right $ Result did oldLbl TimedOut)
      putOk = putMVar mv (Right $ Result did (Just lbl) Ok)
  setOneLightLabel lc lbl cl putTimeout putOk
  return mv


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

instance Connection LanConnection where
  listLights lc sel needed = do
    result <- doListLights lc sel needed
    listOfMVarToList result

  setStates lc pairs = do
    mapM_ (checkTransition . snd) pairs
    result <- doSetStates lc pairs
    listOfMVarToList result

  effect lc sel eff = do
    checkEffect eff
    result <- doEffect lc sel eff
    listOfMVarToList result

  listScenes lc = lsListScenes $ lcSettings lc

  setLabel lc dev lbl = do
    lites <- atomically $ readTVar $ lcLights lc
    case dev `M.lookup` lites of
     Nothing -> throwIO $ SelectorNotFound $ SelDevId dev
     (Just lite) -> do
       now <- dateCurrent
       let oi = lsOfflineInterval $ lcSettings lc
       mv <- checkAlive now oi (labelOneLight lc lbl) offlineResult lite
       takeMVarThrow mv

  closeConnection lc =
    endThread (lsLog $ lcSettings lc) "discovery" (lcThread lc)

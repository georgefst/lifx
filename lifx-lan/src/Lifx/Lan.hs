module Lifx.Lan
    ( LanSettings(..)
    , LanConnection
    , openLanConnection
    , getLan
    ) where

import Lifx
import Lifx.Lan.LowLevel

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Data.Bits
import Data.Hourglass
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Version
import Data.Word
import System.Hourglass
import System.IO.Unsafe
import System.Mem.Weak

data LanSettings =
  LanSettings
  { lsIfName      :: T.Text
  , lsLog         :: String -> IO ()
  , lsPort        :: !Word16
  , lsListScenes  :: IO [Scene]
  , lsRetryParams :: RetryParams
  }

data CachedThing a = NotCached | Cached DateTime a deriving (Show, Eq, Ord)

data CachedLight =
  CachedLight
  { clBulb     :: Bulb
  , clLocation :: CachedThing LocationId
  , clGroup    :: CachedThing GroupId
  , clLabel    :: CachedThing Label
  } deriving (Show, Eq, Ord)

data CachedLabel =
  CachedLabel
  { claLabel     :: Label
  , claUpdatedAt :: !Word64
  } deriving (Show, Eq, Ord)

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

openLanConnection :: LanSettings -> IO LanConnection
openLanConnection ls = do
  lan <- openLan' (lsIfName ls) (Just $ lsPort ls) (Just $ lsLog ls)
  m1 <- tvEmptyMap
  m2 <- tvEmptyMap
  m3 <- tvEmptyMap
  tmv <- newEmptyTMVarIO
  thr <- forkIO (discoveryThread tmv)
  wthr <- mkWeakThreadId thr
  atomically $ do
    let lc = LanConnection lan ls m1 m2 m3 wthr
    putTMVar tmv lc
    return lc

getLan :: LanConnection -> Lan
getLan = lcLan

data MVarList a = Empty | Cons { car :: a , cdr :: MVar (MVarList a) }
                deriving Eq

mVarListToList :: MVar (MVarList a) -> IO [a]
mVarListToList mvl = unsafeInterleaveIO $ do
  l <- takeMVar mvl
  case l of
   Empty -> return []
   Cons lHead lTail -> do
     lTail' <- mVarListToList lTail
     return (lHead : lTail')

emptyLightInfo :: DeviceId -> DateTime -> LightInfo
emptyLightInfo devId now = LightInfo
  { lId = devId
  , lUuid = Nothing
  , lLabel = Nothing
  , lConnected = True
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

selToNeeded :: Selector -> [InfoNeeded]
selToNeeded SelAll             = []
selToNeeded (SelLabel _ )      = [NeedLabel]
selToNeeded (SelDevId _ )      = []
selToNeeded (SelGroup _ )      = [NeedGroup]
selToNeeded (SelGroupId _ )    = [NeedGroup]
selToNeeded (SelLocation _ )   = [NeedLocation]
selToNeeded (SelLocationId _ ) = [NeedLocation]


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


whatsNeeded :: [Selector] -> [InfoNeeded] -> [MessageNeeded]
whatsNeeded sel needed =
  sort $ nub $ map needMessage $ concatMap selToNeeded sel ++ needed


type FinCont = Maybe LightInfo -> IO ()
type NxtCont = LightInfo -> IO ()

selLight :: Selector -> StateLight -> Bool
selLight (SelLabel lbl) sl = lbl == slLabel sl
selLight _ _ = True

selGroup :: Selector -> StateGroup -> Bool
selGroup (SelGroup lbl) sg = lbl == sgLabel sg -- FIXME: use up-to-date label
selGroup (SelGroupId gid) sg = gid == sgGroup sg
selGroup _ _ = True

selLocation :: Selector -> StateLocation -> Bool
selLocation (SelLocation lbl) slo = lbl == sloLabel slo -- FIXME: use up-to-date label
selLocation (SelLocationId lid) slo = lid == sloLocation slo
selLocation _ _ = True

color16toFrac :: HSBK16 -> Color
color16toFrac c = HSBK
  { hue = fromIntegral (hue c) / 65535 * 360
  , saturation = fromIntegral (saturation c) / 65535
  , brightness = fromIntegral (brightness c) / 65535
  , kelvin = fromIntegral (kelvin c)
  }

justColor :: Color -> MaybeColor
justColor = fmap Just

color16ToMaybeFrac :: HSBK16 -> MaybeColor
color16ToMaybeFrac hsbk = justColor $ color16toFrac hsbk

nanosPerSecond :: FracSeconds
nanosPerSecond = 1e9

unpackFirmwareVersion :: Word32 -> Version
unpackFirmwareVersion v = Version (map fromIntegral [major, minor]) []
  where major = v `shiftR` 16
        minor = v .&. 0xffff

discoveryCallback :: LanConnection
                     -> [Selector]
                     -> [MessageNeeded]
                     -> TVar (Maybe (MVar (MVarList LightInfo)))
                     -> DateTime
                     -> Bulb
                     -> IO ()
discoveryCallback lc sels messagesNeeded tv now bulb =
  gatherInfo (lcSettings lc, bulb, sels, fin) messagesNeeded eli

  where eli = emptyLightInfo (deviceId bulb) now

        gatherInfo _ [] li = fin (Just li)
        gatherInfo stuff (mneed:mneeds) li =
          cbForMessage stuff mneed (gatherInfo stuff mneeds) li

        fin Nothing = return ()
        fin (Just li) = appendLightInfo tv li

appendLightInfo :: TVar (Maybe (MVar (MVarList LightInfo)))
                   -> LightInfo
                   -> IO ()
appendLightInfo tv li = do
  mv <- newEmptyMVar
  f <- atomically $ do
    mby <- readTVar tv
    case mby of
     (Just mvPrev) -> do
       writeTVar tv (Just mv)
       return $ putMVar mvPrev $ Cons { car = li , cdr = mv }
     Nothing -> return $ return ()
  f

cbForMessage :: (LanSettings, Bulb, [Selector], FinCont)
                -> MessageNeeded
                -> NxtCont
                -> LightInfo
                -> IO ()
cbForMessage (ls, bulb, sels, finCont) mneed nxtCont li = f mneed
  where rq q cb = reliableQuery (lsRetryParams ls) (q bulb) cb $ do
                    (lsLog ls) (show bulb ++ " not responding to " ++ opName)
                    finCont (Just li)
        opName = drop 4 $ show mneed

        f NeedGetLight        = rq getLight        cbLight
        f NeedGetGroup        = rq getGroup        cbGroup
        f NeedGetLocation     = rq getLocation     cbLocation
        f NeedGetVersion      = rq getVersion      cbVersion
        f NeedGetHostInfo     = rq getHostInfo     cbHostInfo
        f NeedGetInfo         = rq getInfo         cbInfo
        f NeedGetHostFirmware = rq getHostFirmware cbHostFirmware

        sel = head (sels ++ [SelAll]) -- FIXME: support multiple selectors

        cbLight sl = if selLight sel sl
                     then nxtCont (trLight sl)
                     else finCont Nothing
        cbGroup sg = if selGroup sel sg
                     then nxtCont (trGroup sg)
                     else finCont Nothing
        cbLocation slo = if selLocation sel slo
                         then nxtCont (trLocation slo)
                         else finCont Nothing
        cbVersion sv       = nxtCont (trVersion sv)
        cbHostInfo shi     = nxtCont (trHostInfo shi)
        cbInfo si          = nxtCont (trInfo si)
        cbHostFirmware shf = nxtCont (trHostFirmware shf)

        trLight sl = li { lColor = color16ToMaybeFrac (slColor sl)
                        , lPower = Just (slPower sl)
                        , lLabel = Just (slLabel sl)
                        }
        trGroup sg = li { lGroupId = Just (sgGroup sg)
                        , lGroup   = Just (sgLabel sg)
                        }
        trLocation slo = li { lLocationId = Just (sloLocation slo)
                            , lLocation   = Just (sloLabel slo)
                            }
        trVersion sv = li { lProduct = productFromId (svVendor sv) (svProduct sv)
                          , lHardwareVersion = Just (fromIntegral $ svVersion sv)
                          }
        trHostInfo shi = li { lTemperature = Just (fromIntegral (shiMcuTemperature shi) / 100) }
        trInfo si = li { lUptime = Just (fromIntegral (siUptime si) / nanosPerSecond) }
        trHostFirmware shf = li { lFirmwareVersion = Just (unpackFirmwareVersion $ shfVersion shf) }

onceCb :: TVar (S.Set DeviceId) -> (Bulb -> IO ()) -> Bulb -> IO ()
onceCb done realCb bulb = do
  let dev = deviceId bulb
  dup <- atomically $ do
    s <- readTVar done
    let d = dev `S.member` s
    unless d $ writeTVar done $ dev `S.insert` s
    return d
  unless dup $ realCb bulb

selectLights :: LanConnection -> Selector -> STM [Bulb]
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
                -> STM [Bulb]
filterLights tv f = do
  m <- readTVar tv
  let lites = M.elems m
  return $ map clBulb $ filter (isAccept . f) lites

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


doListLights :: LanConnection
                -> [Selector]
                -> [InfoNeeded]
                -> MVar (MVarList LightInfo)
                -> IO ()
doListLights lc sels needed result = do
  let messagesNeeded = whatsNeeded sels needed
  s <- newTVarIO S.empty
  tv <- newTVarIO (Just result)
  now <- dateCurrent
  forM_ [1..15] $ \_ -> do
    discoverBulbs (lcLan lc)
      $ onceCb s
      $ discoveryCallback lc sels messagesNeeded tv now
    threadDelay 100000
  mv <- atomically $ do
    x <- readTVar tv
    writeTVar tv Nothing
    return x
  putMVar (fromJust mv) Empty

updateLabelCache :: Ord a
                    => TVar (M.Map a CachedLabel)
                    -> a
                    -> Label
                    -> Word64
                    -> STM ()
updateLabelCache tv key lbl upd = do
  cache <- readTVar tv
  let doUpdate = case key `M.lookup` cache of
                  Nothing -> True
                  (Just (CachedLabel { claUpdatedAt = oldUpd })) -> upd > oldUpd
      cl = CachedLabel { claLabel = lbl , claUpdatedAt = upd }
      cache' = M.insert key cl cache
  when doUpdate $ writeTVar tv cache'

longAgo = DateTime d t
  where d = Date 1776 July 4
        t = TimeOfDay (Hours 0) (Minutes 0) (Seconds 0) (NanoSeconds 0)

dtOfCt :: CachedThing a -> DateTime
dtOfCt NotCached = longAgo
dtOfCt (Cached dt _ ) = dt

microsPerSecond = 1e6
discoveryTime = 1.5
fastDiscoveryTime = 0.25

discoveryThread :: TMVar LanConnection -> IO ()
discoveryThread tmv = do
  lc <- atomically $ takeTMVar tmv
  forM_ [1..3] $ \_ -> do
    db lc
    td fastDiscoveryTime
  forever $ do
    db lc
    td discoveryTime
  where
    db lc = discoverBulbs (lcLan lc) $ discoveryCb lc
    td secs = threadDelay $ floor $ microsPerSecond * secs

data Query = QueryLocation | QueryGroup | QueryLabel deriving (Show, Eq, Ord)

discoveryCb :: LanConnection -> Bulb -> IO ()
discoveryCb lc bulb = do
  queries <- atomically $ do
    lites <- readTVar (lcLights lc)
    case deviceId bulb `M.lookup` lites of
     Nothing -> do
       let lite = CachedLight bulb NotCached NotCached NotCached
       writeTVar (lcLights lc) $ M.insert (deviceId bulb) lite lites
       return [ QueryLocation , QueryGroup , QueryLabel ] -- update all three
     (Just lite) ->
       -- FIXME: what if bulb changes (same id, new ip)
       let pairs = [ (dtOfCt (clLocation lite), QueryLocation)
                   , (dtOfCt (clGroup    lite), QueryGroup)
                   , (dtOfCt (clLabel    lite), QueryLabel) ]
       in return [ snd $ head $ sort pairs ] -- just update the oldest one
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

instance Connection LanConnection where
  listLights lc sel needed = do
    result <- newEmptyMVar
    forkIO $ doListLights lc sel needed result
    mVarListToList result

  setStates lc pairs = undefined
  togglePower lc sel dur = undefined
  effect lc sel eff = undefined
  listScenes lc = undefined
  activateScene lc scene dur = undefined
  cycleLights lc sel states = undefined

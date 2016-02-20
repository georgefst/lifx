{-# LANGUAGE OverloadedStrings #-}

module Lifx.Lan
    ( LanSettings(..)
    , LanConnection
    , defaultLanSettings
    , openLanConnection
    , getLan
    ) where

import Lifx
import qualified Lifx as E( EffectType( Pulse ) )
import Lifx.Lan.LowLevel
import qualified Lifx.Lan.LowLevel as W( Waveform( Pulse ) )

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
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
import System.Hourglass
import System.IO
import System.IO.Unsafe
import System.Mem.Weak

data LanSettings =
  LanSettings
  { lsIfName      :: T.Text
  , lsLog         :: T.Text -> IO ()
  , lsPort        :: !Word16
  , lsListScenes  :: IO [Scene]
  , lsRetryParams :: RetryParams
  }

defaultLanSettings :: LanSettings
defaultLanSettings =
  LanSettings
  { lsIfName      = "en1"
  , lsLog         = TIO.hPutStrLn stderr
  , lsPort        = 56700
  , lsListScenes  = return []
  , lsRetryParams = defaultRetryParams
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

listOfMVarToList :: [MVar a] -> IO [a]
listOfMVarToList = mapM (unsafeInterleaveIO . takeMVar)

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

colorFracTo16 :: Color -> HSBK16
colorFracTo16 c = HSBK
  { hue = round $ hue c * 65535 / 360
  , saturation = round $ saturation c * 65535
  , brightness = round $ brightness c * 65535
  , kelvin = round $ kelvin c
  }

justColor :: Color -> MaybeColor
justColor = fmap Just

definitelyColor :: MaybeColor -> Color
definitelyColor = fmap fromJust

color16ToMaybeFrac :: HSBK16 -> MaybeColor
color16ToMaybeFrac hsbk = justColor $ color16toFrac hsbk

f2ms :: LiFrac -> Word32
f2ms x = round $ 1000 * x

f2µs :: LiFrac -> Int
f2µs x = round $ 1e6 * x

nanosPerSecond :: FracSeconds
nanosPerSecond = 1e9

unpackFirmwareVersion :: Word32 -> Version
unpackFirmwareVersion v = Version (map fromIntegral [major, minor]) []
  where major = v `shiftR` 16
        minor = v .&. 0xffff

lastSeen :: CachedLight -> DateTime
lastSeen cl = maximum [ dtOfCt (clLocation cl)
                      , dtOfCt (clGroup    cl)
                      , dtOfCt (clLabel    cl)
                      ]

listOneLight :: LanConnection
                -> [Selector]
                -> [MessageNeeded]
                -> TVar (Maybe (MVar (MVarList LightInfo)))
                -> TSem
                -> CachedLight
                -> IO ()
listOneLight lc sels messagesNeeded tv sem cl =
  gatherInfo (lcSettings lc, bulb, sels, fin) messagesNeeded eli

  where bulb = clBulb cl
        eli = emptyLightInfo (deviceId bulb) (lastSeen cl)

        gatherInfo _ [] li = fin (Just li)
        gatherInfo stuff (mneed:mneeds) li =
          cbForMessage stuff mneed (gatherInfo stuff mneeds) li

        fin Nothing = sigSem
        fin (Just li) = do
          appendLightInfo tv li
          sigSem

        sigSem = atomically $ signalTSem sem

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
                    (lsLog ls)
                      (T.pack $ show bulb ++ " not responding to " ++ opName)
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

applySelectors :: LanConnection -> [Selector] -> STM [CachedLight]
applySelectors lc sels = do
  lists <- mapM (selectLights lc) sels
  let sets = map S.fromList lists
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


doListLights :: LanConnection
                -> [Selector]
                -> [InfoNeeded]
                -> MVar (MVarList LightInfo)
                -> IO ()
doListLights lc sels needed result = do
  let messagesNeeded = whatsNeeded sels needed
  tv <- newTVarIO (Just result)
  sem <- atomically $ newTSem 0
  lites <- atomically $ applySelectors lc sels
  forM_ lites $ listOneLight lc sels messagesNeeded tv sem
  atomically $ forM_ lites $ \_ -> waitTSem sem
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

doSetStates :: LanConnection
               -> [([Selector], StateTransition)]
               -> IO [MVar StateTransitionResult]
doSetStates lc pairs = forM pairs $ setOneState lc

setOneState :: LanConnection
               -> ([Selector], StateTransition)
               -> IO (MVar StateTransitionResult)
setOneState lc pair@(sels, st) = do
  mv <- newEmptyMVar
  forkIO $ do
    lites <- atomically $ applySelectors lc sels
    results <- forM lites $ setOneLightState lc st
    results' <- listOfMVarToList results
    putMVar mv (StateTransitionResult pair results')
  return mv

setOneLightState :: LanConnection
                    -> StateTransition
                    -> CachedLight
                    -> IO (MVar Result)
setOneLightState lc st cl = do
  mv <- newEmptyMVar
  let did = deviceId (clBulb cl)
      lbl = maybeFromCached (clLabel cl)
      putResult stat = putMVar mv (Result did lbl stat)
      timeout = putResult TimedOut
      newColor = sColor st
      skipGet = isEmptyColor newColor || isCompleteColor newColor
  getOneLight lc skipGet cl timeout $
    \( _ , oldColor ) -> setOneLightColor lc oldColor newColor (sDuration st) cl timeout $
      setOneLightPower lc (sPower st) (sDuration st) cl timeout $
        putResult Ok
  return mv

maybeFromCached :: CachedThing a -> Maybe a
maybeFromCached NotCached = Nothing
maybeFromCached (Cached _ x) = Just x

getOneLight :: LanConnection
               -> Bool
               -> CachedLight
               -> IO ()
               -> ((Power, MaybeColor) -> IO ())
               -> IO ()
getOneLight _ True _ _ cbSucc = cbSucc (Off, emptyColor)
getOneLight lc False cl cbFail cbSucc =
  reliableQuery rp (getLight bulb) succ cbFail
  where rp = lsRetryParams $ lcSettings lc
        bulb = clBulb cl
        succ sl = cbSucc (slPower sl, color16ToMaybeFrac $ slColor sl)

setOneLightColor :: LanConnection
                    -> MaybeColor
                    -> MaybeColor
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
        c = (colorFracTo16 $ definitelyColor combinedColor)

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

blockingQuery :: RetryParams
                 -> ((a -> IO ()) -> IO ())
                 -> IO (Maybe a)
blockingQuery rp query = do
  mv <- newEmptyMVar
  let cbSucc x = putMVar mv (Just x)
      cbFail = putMVar mv Nothing
  reliableQuery rp query cbSucc cbFail
  takeMVar mv

blockingAction :: RetryParams
                  -> (IO () -> IO ())
                  -> IO (Maybe ())
blockingAction rp action =
  blockingQuery rp query
  where query cb = action $ cb ()

doTogglePower :: LanConnection
                 -> [Selector]
                 -> FracSeconds
                 -> IO [MVar Result]
doTogglePower lc sels dur = do
  lites <- atomically $ applySelectors lc sels
  forM lites $ toggleOneLightPower lc dur

toggleOneLightPower :: LanConnection
                       -> FracSeconds
                       -> CachedLight
                       -> IO (MVar Result)
toggleOneLightPower lc dur cl = do
  mv <- newEmptyMVar
  let did = deviceId (clBulb cl)
      lbl = maybeFromCached (clLabel cl)
      putResult stat = putMVar mv (Result did lbl stat)
      timeout = putResult TimedOut
  getOneLight lc False cl timeout $
    \( pwr , _ ) -> setOneLightPower lc (Just $ flipPwr pwr) dur cl timeout $
                     putResult Ok
  return mv

flipPwr On = Off
flipPwr Off = On

doEffect :: LanConnection
            -> [Selector]
            -> Effect
            -> IO [MVar Result]
doEffect lc sels eff = do
  lites <- atomically $ applySelectors lc sels
  forM lites $ effectOneLight lc eff

effectOneLight :: LanConnection
                  -> Effect
                  -> CachedLight
                  -> IO (MVar Result)
effectOneLight lc eff cl = do
  mv <- newEmptyMVar
  let did = deviceId (clBulb cl)
      lbl = maybeFromCached (clLabel cl)
      putResult stat = putMVar mv (Result did lbl stat)
      timeout = putResult TimedOut
      nd2setColor = not $ isEmptyColor $ eFromColor eff
      nd2combineColor = (not ((isEmptyColor $ eFromColor eff) ||
                              (isCompleteColor $ eFromColor eff))) ||
                        (not $ isCompleteColor $ eColor eff)
      nd2restoreColor = nd2setColor && not (ePersist eff)
      nd2getPwr = ePowerOn eff
      skipGet = not nd2restoreColor && not nd2getPwr && not nd2combineColor
  getOneLight lc skipGet cl timeout $
    \(origPwr, origColor) ->
      setOneLightColor lc origColor (eFromColor eff) 0 cl timeout $
        let nd2ChangePwr = ePowerOn eff && (origPwr == Off)
            (newPwr, restorePwr) = if nd2ChangePwr
                                   then (Just On, Just Off)
                                   else (Nothing, Nothing)
        in setOneLightPower lc newPwr 0 cl timeout $
           setOneLightWaveform lc origColor eff cl timeout $
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
                       -> MaybeColor
                       -> Effect
                       -> CachedLight
                       -> IO ()
                       -> IO ()
                       -> IO ()
setOneLightWaveform lc origColor eff cl cbFail cbSucc =
  reliableAction rp (setWaveform bulb swf) cbSucc cbFail
  where rp = lsRetryParams $ lcSettings lc
        bulb = clBulb cl
        combinedColor = origColor `combineColors` eColor eff
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

instance Connection LanConnection where
  listLights lc sel needed = do
    result <- newEmptyMVar
    forkIO $ doListLights lc sel needed result
    mVarListToList result

  setStates lc pairs = do
    result <- doSetStates lc pairs
    listOfMVarToList result

  togglePower lc sel dur = do
    result <- doTogglePower lc sel dur
    listOfMVarToList result

  effect lc sel eff = do
    result <- doEffect lc sel eff
    listOfMVarToList result

  listScenes lc = lsListScenes $ lcSettings lc

  activateScene lc scene dur = undefined
  cycleLights lc sel states = undefined

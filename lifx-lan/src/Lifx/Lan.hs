module Lifx.Lan where

import Lifx
import Lifx.Lan.LowLevel

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Data.Hourglass
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Version
import Data.Word
import System.IO.Unsafe

data LanSettings =
  LanSettings
  { lsIfName      :: T.Text
  , lsLog         :: String -> IO ()
  , lsPort        :: !Word16
  , lsListScenes  :: IO [Scene]
  , lsRetryParams :: RetryParams
  }

data LanConnection =
  LanConnection
  { lcLan :: Lan
  , lcSettings :: LanSettings
  }

instance Show LanConnection where
  show (LanConnection { lcLan = lan }) = show lan

instance Eq LanConnection where
  x1 == x2 = x1 `compare` x2 == EQ

instance Ord LanConnection where
  (LanConnection { lcLan = lan1 }) `compare` (LanConnection { lcLan = lan2 }) =
    lan1 `compare` lan2

openLanConnection :: LanSettings -> IO LanConnection
openLanConnection ls = do
  lan <- openLan' (lsIfName ls) (Just $ lsPort ls) (Just $ lsLog ls)
  return $ LanConnection lan ls

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
selLight = undefined

selGroup :: Selector -> StateGroup -> Bool
selGroup = undefined

selLocation :: Selector -> StateLocation -> Bool
selLocation = undefined

todo1 :: HSBK16 -> MaybeColor
todo1 hsbk = undefined

todo2 :: Word32 -> Word32 -> Maybe Product
todo2 vend prod = undefined

todo3 :: FracSeconds
todo3 = undefined

todo4 :: Word32 -> Version
todo4 v = undefined

todoCallback :: [MessageNeeded]
                -> TVar (Maybe (MVar (MVarList LightInfo))) -> Bulb -> IO ()
todoCallback messagesNeeded tv bulb = undefined

cbForMessage :: (LanSettings, Bulb, Selector, FinCont)
                -> MessageNeeded
                -> NxtCont
                -> LightInfo
                -> IO ()
cbForMessage (ls, bulb, sel, finCont) mneed nxtCont li = f mneed
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

        trLight sl = li { lColor = todo1 (slColor sl)
                        , lPower = Just (slPower sl)
                        , lLabel = Just (slLabel sl)
                        }
        trGroup sg = li { lGroupId = Just (sgGroup sg)
                        , lGroup   = Just (sgLabel sg)
                        }
        trLocation slo = li { lLocationId = Just (sloLocation slo)
                            , lLocation   = Just (sloLabel slo)
                            }
        trVersion sv = li { lProduct = todo2 (svVendor sv) (svProduct sv)
                          , lHardwareVersion = Just (fromIntegral $ svVersion sv)
                          }
        trHostInfo shi = li { lTemperature = Just (fromIntegral (shiMcuTemperature shi) / 100) }
        trInfo si = li { lUptime = Just (fromIntegral (siUptime si) / todo3) }
        trHostFirmware shf = li { lFirmwareVersion = Just (todo4 $ shfVersion shf) }

doListLights :: LanConnection
                -> [Selector]
                -> [InfoNeeded]
                -> MVar (MVarList LightInfo)
                -> IO ()
doListLights lc sel needed result = do
  let messagesNeeded = whatsNeeded sel needed
  tv <- newTVarIO (Just result)
  forM_ [1..15] $ \_ -> do
    discoverBulbs (lcLan lc) (todoCallback messagesNeeded tv)
    threadDelay 100000
  mv <- atomically $ do
    x <- readTVar tv
    writeTVar tv Nothing
    return x
  putMVar (fromJust mv) Empty

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

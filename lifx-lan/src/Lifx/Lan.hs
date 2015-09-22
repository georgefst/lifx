module Lifx.Lan where

import Lifx
import Lifx.Lan.LowLevel

import Control.Concurrent.MVar

data LanSettings =
  LanSettings
  { lsIfName :: T.Text
    lsLog :: String -> IO ()
    lsPort :: Word16
    lsListScenes :: IO [Scene]
  }

data LanConnection =
  LanConnection
  { lcLan :: Lan
    lcSettings :: LanSettings
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
  lan <- openLan' (lsIfName ls) (lsPort ls) (Just $ lsLog ls)
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

doListLights :: LanConnection
                -> Selector
                -> [InfoNeeded]
                -> MVar (MVarList LightInfo)
                -> IO ()
doListLights lc sel needed result = do
  tv <- newTVarIO (Just result)
  forM_ [1..15] $ \_ -> do
    discoverBulbs (lcLan lc) todoCallback
    threadDelay 100000
  mv <- atomically $ do
    x <- readTVar tv
    writeTVar tv Nothing
    return x
  putMVar mv Empty

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

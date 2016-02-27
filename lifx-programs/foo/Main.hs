{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Monad
import Data.Char
import qualified Data.Text as T
import Test.Tasty.HUnit

import Lifx.Lan
import Lifx.Lan.LowLevel
import Lifx
import Lifx.Cloud

st = StateTransition
     { sPower = Nothing
     , sColor = blue
     , sDuration = 1.0
     }

st2 = StateTransition
      { sPower = Nothing
      , sColor = white
      , sDuration = 1.0
      }

fromRight = either error id

justColor :: Color -> MaybeColor
justColor = fmap Just

dly = threadDelay 1000000

chkTransitionResult :: String
                    -> (([Selector], StateTransition), StateTransitionResult, Int)
                    -> IO ()
chkTransitionResult msg (op@(sels, trans), tr, idx) = do
  let msg' = msg ++ ": result index " ++ show idx
  assertEqual (msg' ++ ": tOperation") op (tOperation tr)
  forM_ (tResults tr) $ \r ->
    assertEqual (msg' ++ ": device " ++ show (rId r)) Ok (rStatus r)

setStates' :: Connection c
              => c
              -> String
              -> [([Selector], StateTransition)]
              -> IO [StateTransitionResult]
setStates' conn msg pairs = do
  trs <- setStates conn pairs
  assertEqual (msg ++ ": result length") (length pairs) (length trs)
  let triples = zip3 pairs trs [0..]
  mapM_ (chkTransitionResult msg) triples
  return trs

main = do
  {-
  lc <- openLanConnection defaultLanSettings
  threadDelay 1000000
  -}

  let devs = map (fromRight . fromText) ["d073d5029e03", "d073d502b95f"]
      sels = map SelDevId devs

  lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
  let lifxToken = fromRight $ fromText $ T.pack $ takeWhile (not . isSpace) lifxTokenStr
      cs = defaultCloudSettings { csToken = lifxToken }
  lc <- openCloudConnection cs
  li <- listLights lc sels needEverything
  print li
  putStrLn ""

  setStates' lc "stuff"
    [(sels, StateTransition { sPower = Just On
                            , sColor = justColor $ HSBK 0 0 1 5000
                            , sDuration = 0
                            })]

  dly

  {-
  tr <- setStates lc [([SelAll], st)]
  tr <- togglePower lc [SelAll] 3.0
  tr <- effect lc [SelAll] defaultEffect { eColor = red
                                         , eType = Breathe
                                         , eCycles = 5
                                         , eFromColor = green
                                         , ePowerOn = True
                                         }
  tr <- listScenes lc
  let scn = fromRight $ fromText "44f42e8c-96fe-4663-a280-a72e65249162"
  tr <- activateScene lc scn 10.0
  -}
  tr <- cycleLights lc sels [st, st2]
  print tr
  closeConnection lc

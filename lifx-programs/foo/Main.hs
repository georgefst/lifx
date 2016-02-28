{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Test.Tasty
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

definitelyColor :: MaybeColor -> Color
definitelyColor = fmap fromJust

dly = threadDelay 100000

chkTransitionResult :: String
                    -> (([Selector], StateTransition), StateTransitionResult, Int)
                    -> IO ()
chkTransitionResult msg (op, tr, idx) = do
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

chkTransitionResultDevId :: String
                            -> (([DeviceId], StateTransition), StateTransitionResult, Int)
                            -> IO ()
chkTransitionResultDevId msg ((devs, _), tr, idx) = do
  let msg' = msg ++ ": result index " ++ show idx
      results = tResults tr
  assertEqual (msg ++ ": length") (length devs) (length results)
  forM_ (zip3 devs results [1..]) $ \(d, r, i) ->
    assertEqual (msg' ++ ": device index " ++ show i) d (rId r)

setStatesDevId :: Connection c
                  => c
                  -> String
                  -> [([DeviceId], StateTransition)]
                  -> IO [StateTransitionResult]
setStatesDevId conn msg pairs = do
  trs <- setStates' conn msg $ map (first (map SelDevId)) pairs
  let triples = zip3 pairs trs [0..]
  mapM_ (chkTransitionResultDevId msg) triples
  return trs

main = do
  {-
  lc <- openLanConnection defaultLanSettings
  threadDelay 1000000
  -}

  let devs = map (fromRight . fromText) ["d073d5029e03", "d073d502b95f"]

  lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
  let lifxToken = fromRight $ fromText $ T.pack $ takeWhile (not . isSpace) lifxTokenStr
      cs = defaultCloudSettings { csToken = lifxToken }

  lc <- openCloudConnection cs

  defaultMain $ testCaseSteps "list lights" (testListLights lc devs)

  closeConnection lc

defaultColor :: Color
defaultColor = HSBK 0 0 0.5 5000

knownState :: Connection c
              => c
              -> [DeviceId]
              -> (String -> IO ())
              -> IO ()
knownState conn devs step = do
  step "reset to white"
  setStatesDevId conn "reset to white"
    [(devs, StateTransition { sPower = Just On
                            , sColor = justColor $ defaultColor
                            , sDuration = 0
                            })]
  dly

fst3 :: (a, b, c) -> a
fst3 (x, _, _ ) = x

assertCloseEnough :: (Num a, Ord a, Show a)
                     => a
                     -> String
                     -> a
                     -> a
                     -> IO ()
assertCloseEnough fudge msg expected actual =
  if abs (expected - actual) < fudge
  then return ()
  else assertFailure (msg ++ ": " ++ show expected ++ " and " ++
                      show actual ++ " not within " ++ show fudge)

assertColorEqual :: String -> Color -> Color -> IO ()
assertColorEqual msg expected actual = do
  assertCloseEnough (360 / 65534) (msg ++ ": hue") (hue expected) (hue actual)
  assertCloseEnough (1 / 65534) (msg ++ ": saturation") (saturation expected) (saturation actual)
  assertCloseEnough (1 / 65534) (msg ++ ": brightness") (brightness expected) (brightness actual)
  assertCloseEnough 0.5 (msg ++ ": kelvin") (kelvin expected) (kelvin actual)

checkOneColor :: ((DeviceId, Power, Color), LightInfo)
                 -> IO ()
checkOneColor ((did, pwr, color), linfo) = do
  assertEqual "checkColor" did (lId linfo)
  assertEqual "checkColor" (Just pwr) (lPower linfo)
  assertColorEqual "checkColor" color (definitelyColor $ lColor linfo)

checkColor :: [(DeviceId, Power, Color)]
              -> [LightInfo]
              -> IO ()
checkColor triple linfo = do
  assertEqual "checkColor length" (length triple) (length linfo)
  let triple' = sortBy (comparing fst3) triple
      linfo' = sortBy (comparing lId) linfo
      pairs = zip triple' linfo'
  mapM_ checkOneColor pairs

testListLights :: (Connection c)
                  => c
                  -> [DeviceId]
                  -> (String -> IO ())
                  -> IO ()
testListLights conn devs step = do
  knownState conn devs step
  step "listing lights"

  let sels = map SelDevId devs
  li <- listLights conn sels needEverything
  checkColor (zip3 devs (repeat On) (repeat defaultColor)) li
  -- print li
  -- putStrLn ""

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

  -- step "cycleLights"

  -- cycleLights conn sels [st, st2]
  -- return ()
  -- print tr

  -- step "closing connection"

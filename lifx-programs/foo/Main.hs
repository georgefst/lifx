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

import Lifx
import Lifx.Cloud
import Lifx.Lan

fromRight = either error id

justColor :: Color -> MaybeColor
justColor = fmap Just

definitelyColor :: MaybeColor -> Color
definitelyColor = fmap fromJust

dly = threadDelay 500000

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
  forM_ (zip (sort devs) (sort results)) $ \(d, r) ->
    assertEqual msg' d (rId r)

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

someTests :: (Connection c1, Connection c2)
             => c1
             -> c2
             -> [DeviceId]
             -> [TestTree]
someTests conn1 conn2 devs =
  [ testCaseSteps "list lights"  (testListLights  conn1 conn2 devs)
  , testCaseSteps "toggle power" (testTogglePower conn1 conn2 devs)
  , testCaseSteps "set states (power)" (testSetPower conn1 conn2 devs)
  , testCaseSteps "set states (hue and saturation)" (testSetStatesHS conn1 conn2 devs)
  , testCaseSteps "set states (brightness)" (testSetStatesB conn1 conn2 devs)
  , testCaseSteps "set states (kelvin)"     (testSetStatesK conn1 conn2 devs)
  , testCaseSteps "set states (saturation and kelvin)" (testSetStatesSK conn1 conn2 devs)
  , testCaseSteps "set states (hue and power)" (testSetStatesHP conn1 conn2 devs)
  , testGroup "pulse effect"   (effectTests conn1 conn2 devs Pulse)
  , testGroup "breathe effect" (effectTests conn1 conn2 devs Breathe)
  ]

effectTests :: (Connection c1, Connection c2)
               => c1
               -> c2
               -> [DeviceId]
               -> EffectType
               -> [TestTree]
effectTests conn1 conn2 devs typ =
  [ testCaseSteps "effect" (testEffect conn1 conn2 devs typ)
  , testCaseSteps "effect (persist)" (testEffectPersist conn1 conn2 devs typ)
  ]

main = do
  lc <- openLanConnection defaultLanSettings
  threadDelay 1000000

  let devs = map (fromRight . fromText) ["d073d5029e03", "d073d502b95f"]

  lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
  let lifxToken = fromRight $ fromText $ T.pack $ takeWhile (not . isSpace) lifxTokenStr
      cs = defaultCloudSettings { csToken = lifxToken }

  cc <- openCloudConnection cs

  defaultMain $ testGroup "Tests"
    [ testGroup "Cloud" (someTests cc cc devs)
    , testGroup "Lan"   (someTests lc lc devs)
    , testGroup "CloudAndLan" (someTests cc lc devs)
    , testGroup "LanAndCloud" (someTests lc cc devs)
    ]

  closeConnection lc
  closeConnection cc

-- LIFX cloud seems to not change the hue if the saturation is 0.
-- So, set saturation to 0.1 in order to reset all four components.
-- https://community.lifx.com/t/some-weird-observations-when-writing-automated-tests/1080
defaultColor :: Color
defaultColor = HSBK 0 0.1 0.5 5000

knownState :: Connection c
              => c
              -> [DeviceId]
              -> (String -> IO ())
              -> IO StateTransitionResult
knownState conn devs step = do
  step "reset to white"
  [tr] <- setStatesDevId conn "reset to white"
          [(devs, StateTransition { sPower = Just On
                                  , sColor = justColor defaultColor
                                  , sDuration = 0
                                  })]
  dly
  return tr

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
  -- https://community.lifx.com/t/some-weird-observations-when-writing-automated-tests/1080
  assertCloseEnough (360 / 1000) (msg ++ ": hue") (hue expected) (hue actual)
  assertCloseEnough (1 / 1000) (msg ++ ": saturation") (saturation expected) (saturation actual)
  assertCloseEnough (1 / 1000) (msg ++ ": brightness") (brightness expected) (brightness actual)
  assertCloseEnough 3 (msg ++ ": kelvin") (kelvin expected) (kelvin actual)

checkOneColor :: ((DeviceId, Power, Color), LightInfo)
                 -> IO ()
checkOneColor ((did, pwr, color), linfo) = do
  assertEqual "checkColor" did (lId linfo)
  let msg = "checkColor " ++ show did
  assertEqual msg (Just pwr) (lPower linfo)
  assertColorEqual msg color (definitelyColor $ lColor linfo)

checkColor :: [(DeviceId, Power, Color)]
              -> [LightInfo]
              -> IO ()
checkColor triple linfo = do
  assertEqual "checkColor length" (length triple) (length linfo)
  let triple' = sortBy (comparing fst3) triple
      linfo' = sortBy (comparing lId) linfo
      pairs = zip triple' linfo'
  mapM_ checkOneColor pairs

checkOneLabel :: (Result, LightInfo) -> IO ()
checkOneLabel (result, linfo) = do
  let msg = "checkLabels " ++ show (rId result)
  assertEqual "checkLabels" (rId result) (lId linfo)
  assertEqual msg (rLabel result) (lLabel linfo)
  assertEqual msg Ok (rStatus result)

checkLabels :: [Result] -> [LightInfo] -> IO ()
checkLabels results linfo = do
  assertEqual "checkLabels length" (length results) (length linfo)
  let results' = sortBy (comparing rId) results
      linfo' = sortBy (comparing lId) linfo
      pairs = zip results' linfo'
  mapM_ checkOneLabel pairs

testListLights :: (Connection c1, Connection c2)
                  => c1
                  -> c2
                  -> [DeviceId]
                  -> (String -> IO ())
                  -> IO ()
testListLights conn1 conn2 devs step = do
  tr <- knownState conn1 devs step
  step "listing lights"
  let sels = map SelDevId devs
  li <- listLights conn2 sels needEverything
  checkColor (zip3 devs (repeat On) (repeat defaultColor)) li
  checkLabels (tResults tr) li

testTogglePower :: (Connection c1, Connection c2)
                   => c1
                   -> c2
                   -> [DeviceId]
                   -> (String -> IO ())
                   -> IO ()
testTogglePower conn1 conn2 devs step = do
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs

  step "toggling power (to off)"
  pwrResult <- togglePower conn1 sels 0
  dly

  step "listing lights"
  li <- listLights conn2 sels needEverything
  checkLabels pwrResult li
  checkColor (zip3 devs (repeat Off) (repeat defaultColor)) li
  checkLabels (tResults tr) li

  step "toggling power (to on)"
  pwrResult' <- togglePower conn1 sels 0
  dly

  step "listing lights"
  li' <- listLights conn2 sels needEverything
  checkLabels pwrResult' li'
  checkColor (zip3 devs (repeat On) (repeat defaultColor)) li'
  checkLabels (tResults tr) li'

testSetPower :: (Connection c1, Connection c2)
                => c1
                -> c2
                -> [DeviceId]
                -> (String -> IO ())
                -> IO ()
testSetPower conn1 conn2 devs step = do
  knownState conn1 devs step
  let sels = map SelDevId devs

  step "setting power (to off)"
  [tr] <- setStatesDevId conn1 "power off"
          [(devs, StateTransition { sPower = Just Off
                                  , sColor = emptyColor
                                  , sDuration = 0
                                  })]
  dly

  step "listing lights"
  li <- listLights conn2 sels needEverything
  checkColor (zip3 devs (repeat Off) (repeat defaultColor)) li
  checkLabels (tResults tr) li

  step "setting power (to on)"
  [tr'] <- setStatesDevId conn1 "power on"
           [(devs, StateTransition { sPower = Just On
                                   , sColor = emptyColor
                                   , sDuration = 0
                                   })]
  dly

  step "listing lights"
  li' <- listLights conn2 sels needEverything
  checkColor (zip3 devs (repeat On) (repeat defaultColor)) li'
  checkLabels (tResults tr') li'

stateFromColor :: MaybeColor -> StateTransition
stateFromColor c = StateTransition { sPower = Just On
                                   , sColor = c
                                   , sDuration = 0
                                   }

colors :: [MaybeColor]
colors = [red, orange, yellow, green, cyan, blue, purple, pink]

completeColors :: [Color]
completeColors = map makeComplete colors

makeComplete :: MaybeColor -> Color
makeComplete c = definitelyColor $ combineColors (justColor defaultColor) c

testSetStatesHS :: (Connection c1, Connection c2)
                   => c1
                   -> c2
                   -> [DeviceId]
                   -> (String -> IO ())
                   -> IO ()
testSetStatesHS conn1 conn2 devs step = do
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
  step "setting states"
  setStatesDevId conn1 "setting states"
         $ zip (map (replicate 1) devs) (map stateFromColor colors)
  dly
  step "listing lights"
  li <- listLights conn2 sels needEverything
  checkColor (zip3 devs (repeat On) completeColors) li
  checkLabels (tResults tr) li

testSetStatesB :: (Connection c1, Connection c2)
                  => c1
                  -> c2
                  -> [DeviceId]
                  -> (String -> IO ())
                  -> IO ()
testSetStatesB conn1 conn2 devs step = do
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
  step "setting states"
  let brites = map (\x -> HSBK Nothing Nothing (Just $ x / 10) Nothing) [1..9]
  setStatesDevId conn1 "setting states"
         $ zip (map (replicate 1) devs) (map stateFromColor brites)
  dly
  step "listing lights"
  li <- listLights conn2 sels needEverything
  checkColor (zip3 devs (repeat On) (map makeComplete brites)) li
  checkLabels (tResults tr) li

-- saturation should be set to 0 when kelvin is set
testSetStatesK :: (Connection c1, Connection c2)
                  => c1
                  -> c2
                  -> [DeviceId]
                  -> (String -> IO ())
                  -> IO ()
testSetStatesK conn1 conn2 devs step = do
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
  step "setting states"
  let kelvins = map (\x -> HSBK Nothing Nothing Nothing (Just $ fromInteger x))
                [3000, 3500 .. 6500]
  setStatesDevId conn1 "setting states"
         $ zip (map (replicate 1) devs) (map stateFromColor kelvins)
  dly
  step "listing lights"
  li <- listLights conn2 sels needEverything
  let kelvins' = map (\x -> x { saturation = Just 0 }) kelvins
  checkColor (zip3 devs (repeat On) (map makeComplete kelvins')) li
  checkLabels (tResults tr) li

-- set both kelvin and saturation, to make sure saturation is not
-- overridden if specified explicitly
testSetStatesSK :: (Connection c1, Connection c2)
                   => c1
                   -> c2
                   -> [DeviceId]
                   -> (String -> IO ())
                   -> IO ()
testSetStatesSK conn1 conn2 devs step = do
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
  step "setting states"
  let satkelv = map (\x -> HSBK Nothing (Just 0.5) Nothing (Just $ fromInteger x))
                [3000, 3500 .. 6500]
  setStatesDevId conn1 "setting states"
         $ zip (map (replicate 1) devs) (map stateFromColor satkelv)
  dly
  step "listing lights"
  li <- listLights conn2 sels needEverything
  checkColor (zip3 devs (repeat On) (map makeComplete satkelv)) li
  checkLabels (tResults tr) li

-- set hue and power
testSetStatesHP :: (Connection c1, Connection c2)
                   => c1
                   -> c2
                   -> [DeviceId]
                   -> (String -> IO ())
                   -> IO ()
testSetStatesHP conn1 conn2 devs step = do
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
  step "setting states"
  let hues = map (\x -> HSBK (Just $ fromInteger x) Nothing Nothing Nothing)
             [0, 45 .. 315]
      powers = cycle [On, Off]
  setStatesDevId conn1 "setting states"
         $ zip (map (replicate 1) devs)
               (zipWith3 StateTransition (map Just powers) hues (repeat 0))
  dly
  step "listing lights"
  li <- listLights conn2 sels needEverything
  checkColor (zip3 devs powers (map makeComplete hues)) li
  checkLabels (tResults tr) li

testEffect :: (Connection c1, Connection c2)
              => c1
              -> c2
              -> [DeviceId]
              -> EffectType
              -> (String -> IO ())
              -> IO ()
testEffect conn1 conn2 devs typ step = do
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
      eff = defaultEffect { eType = typ
                          , eColor = blue
                          , ePeriod = 0.2
                          , ePowerOn = False }

  step "performing effect"
  effResult <- effect conn1 sels eff
  dly
  threadDelay 200000

  step "listing lights" -- effect should have had no lasting... uh, effect
  li <- listLights conn2 sels needEverything
  checkLabels effResult li
  checkColor (zip3 devs (repeat On) (repeat defaultColor)) li
  checkLabels (tResults tr) li

testEffectPersist :: (Connection c1, Connection c2)
                     => c1
                     -> c2
                     -> [DeviceId]
                     -> EffectType
                     -> (String -> IO ())
                     -> IO ()
testEffectPersist conn1 conn2 devs typ step = do
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
      eff = defaultEffect { eType = typ
                          , eColor = blue
                          , ePeriod = 0.2
                          , ePersist = True
                          , ePowerOn = False }

  step "performing effect"
  effResult <- effect conn1 sels eff
  dly
  threadDelay 200000

  step "listing lights" -- effect should persist
  li <- listLights conn2 sels needEverything
  checkLabels effResult li
  checkColor (zip3 devs (repeat On) (repeat $ makeComplete blue)) li
  checkLabels (tResults tr) li

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

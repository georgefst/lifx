{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Lifx
import Lifx.Cloud
import Lifx.Lan

fromRight = either error id

fromRight' = either (error . show) id

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
             => IO c1
             -> IO c2
             -> [DeviceId]
             -> [TestTree]
someTests conn1 conn2 devs =
  [ testCaseSteps "list lights"  (testListLights  conn1 conn2 devs)
  , testCaseSteps "toggle power" (testTogglePower conn1 conn2 devs)
  , testCaseSteps "toggle power (mixed)" (testTogglePowerPartial conn1 conn2 devs)
  , testCaseSteps "set state (hsbk)" (testSetStateHSBK conn1 conn2 devs)
  , testCaseSteps "set states (power)" (testSetPower conn1 conn2 devs)
  , testCaseSteps "set states (hue and saturation)" (testSetStatesHS conn1 conn2 devs)
  , testCaseSteps "set states (brightness)" (testSetStatesB conn1 conn2 devs)
  , testCaseSteps "set states (kelvin)"     (testSetStatesK conn1 conn2 devs)
  , testCaseSteps "set states (saturation and kelvin)" (testSetStatesSK conn1 conn2 devs)
  , testCaseSteps "set states (hue and power)" (testSetStatesHP conn1 conn2 devs)
  , testGroup "pulse effect"   (effectTests conn1 conn2 devs
                                defaultEffect { eType = Pulse, eCycles = 1.0 })
  , testGroup "breathe effect" (effectTests conn1 conn2 devs
                                defaultEffect { eType = Breathe, eCycles = 1.5 }
                                ++ breatheOnlyTests conn1 conn2 devs)
  ]

effectTests :: (Connection c1, Connection c2)
               => IO c1
               -> IO c2
               -> [DeviceId]
               -> Effect
               -> [TestTree]
effectTests conn1 conn2 devs eff =
  [ testCaseSteps "effect"           (testEffect        conn1 conn2 devs eff)
  , testCaseSteps "effect (persist)" (testEffectPersist conn1 conn2 devs eff)
  , testCaseSteps "effect (from)"    (testEffectFrom    conn1 conn2 devs eff)
  ]

breatheOnlyTests :: (Connection c1, Connection c2)
                    => IO c1
                    -> IO c2
                    -> [DeviceId]
                    -> [TestTree]
breatheOnlyTests conn1 conn2 devs =
  [ testCaseSteps "breathe (from, persist)"
    (testBreatheFromPersist conn1 conn2 devs)
  , testCaseSteps "breathe (from, persist, hsbk)"
    (testBreatheFromPersistHSBK conn1 conn2 devs)
  ]

main = do
  {-
  let devs = map (fromRight . fromText) ["d073d5029e03", "d073d502b95f"]

  defaultMain $ withResource initCloud closeConnection
    $ \cr -> withResource initLan closeConnection
             $ \lr -> testGroup "Hardware Tests"
                      [ testGroup "Cloud" (someTests cr cr devs)
                      , testGroup "Lan"   (someTests lr lr devs)
                      , testGroup "CloudAndLan" (someTests cr lr devs)
                      , testGroup "LanAndCloud" (someTests lr cr devs)
                      ]
  -}

  defaultMain $ testGroup "color" $
    [ testCase "testRGB" testRGB
    , testCase "selectorTest" selectorTest
    , testCase "selectorsToTextErrorTest" selectorsToTextErrorTest
    ] ++ colorErrorTests ++ selectorErrorTests

  where
    initCloud = do
      lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
      let lifxToken = fromRight $ fromText $ T.pack $ takeWhile (not . isSpace) lifxTokenStr
          cs = defaultCloudSettings { csToken = lifxToken }
      openCloudConnection cs
    initLan = do
      lc <- openLanConnection defaultLanSettings
      threadDelay 1000000
      return lc

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

assertCloseEnough360 :: (Num a, Ord a, Show a)
                        => a
                        -> String
                        -> a
                        -> a
                        -> IO ()
assertCloseEnough360 fudge msg expected actual =
  if abs (expected - actual) < fudge || abs (expected + 360 - actual) < fudge
  then return ()
  else assertFailure (msg ++ ": " ++ show expected ++ " and " ++
                      show actual ++ " not within " ++ show fudge)

assertColorEqual :: String -> Color -> Color -> IO ()
assertColorEqual msg expected actual = do
  -- https://community.lifx.com/t/some-weird-observations-when-writing-automated-tests/1080
  assertCloseEnough360 (360 / 500) (msg ++ ": hue") (hue expected) (hue actual)
  assertCloseEnough (1 / 500) (msg ++ ": saturation") (saturation expected) (saturation actual)
  assertCloseEnough (1 / 500) (msg ++ ": brightness") (brightness expected) (brightness actual)
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

getConnections :: IO a -> IO b -> IO (a, b)
getConnections r1 r2 = do
  c1 <- r1
  c2 <- r2
  return (c1, c2)

testListLights :: (Connection c1, Connection c2)
                  => IO c1
                  -> IO c2
                  -> [DeviceId]
                  -> (String -> IO ())
                  -> IO ()
testListLights rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  step "listing lights"
  let sels = map SelDevId devs
  li <- listLights conn2 sels needEverything
  checkColor (zip3 devs (repeat On) (repeat defaultColor)) li
  checkLabels (tResults tr) li

testTogglePower :: (Connection c1, Connection c2)
                   => IO c1
                   -> IO c2
                   -> [DeviceId]
                   -> (String -> IO ())
                   -> IO ()
testTogglePower rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
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

testTogglePowerPartial :: (Connection c1, Connection c2)
                          => IO c1
                          -> IO c2
                          -> [DeviceId]
                          -> (String -> IO ())
                          -> IO ()
testTogglePowerPartial rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs

  -- FIXME: won't work if there is only one light
  step "toggling power (first light only, to off)"
  pwrResult <- togglePower conn1 (take 1 sels) 0
  dly

  step "listing lights"
  li <- listLights conn2 sels needEverything
  -- checkLabels pwrResult li
  checkColor (zip3 devs (Off : repeat On) (repeat defaultColor)) li
  checkLabels (tResults tr) li

  step "toggling power (all lights, to off)"
  pwrResult' <- togglePower conn1 sels 0
  dly

  step "listing lights"
  li' <- listLights conn2 sels needEverything
  checkLabels pwrResult' li'
  -- https://community.lifx.com/t/toggle-power-endpoint-when-existing-state-is-mixed/1097
  checkColor (zip3 devs (repeat Off) (repeat defaultColor)) li'
  checkLabels (tResults tr) li'

testSetStateHSBK :: (Connection c1, Connection c2)
                    => IO c1
                    -> IO c2
                    -> [DeviceId]
                    -> (String -> IO ())
                    -> IO ()
testSetStateHSBK rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
      myColor = makeComplete cyan

  step "setting state"
  pwrResult <- setState conn1 sels
               $ StateTransition Nothing (justColor myColor) 0
  dly

  step "listing lights"
  li <- listLights conn2 sels [NeedLabel, NeedPower, NeedColor]
  checkLabels pwrResult li
  checkColor (zip3 devs (repeat On) (repeat myColor)) li
  checkLabels (tResults tr) li

testSetPower :: (Connection c1, Connection c2)
                => IO c1
                -> IO c2
                -> [DeviceId]
                -> (String -> IO ())
                -> IO ()
testSetPower rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
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
                   => IO c1
                   -> IO c2
                   -> [DeviceId]
                   -> (String -> IO ())
                   -> IO ()
testSetStatesHS rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
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
                  => IO c1
                  -> IO c2
                  -> [DeviceId]
                  -> (String -> IO ())
                  -> IO ()
testSetStatesB rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
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
                  => IO c1
                  -> IO c2
                  -> [DeviceId]
                  -> (String -> IO ())
                  -> IO ()
testSetStatesK rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
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
                   => IO c1
                   -> IO c2
                   -> [DeviceId]
                   -> (String -> IO ())
                   -> IO ()
testSetStatesSK rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
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
                   => IO c1
                   -> IO c2
                   -> [DeviceId]
                   -> (String -> IO ())
                   -> IO ()
testSetStatesHP rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
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
              => IO c1
              -> IO c2
              -> [DeviceId]
              -> Effect
              -> (String -> IO ())
              -> IO ()
testEffect rsrc1 rsrc2 devs defaultEff step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
      eff = defaultEff { eColor = blue
                       , ePeriod = 0.2
                       , ePowerOn = False
                       }

  step "performing effect"
  effResult <- effect conn1 sels eff
  dly
  threadDelay 300000

  step "listing lights" -- effect should have had no lasting... uh, effect
  li <- listLights conn2 sels needEverything
  checkLabels effResult li
  checkColor (zip3 devs (repeat On) (repeat defaultColor)) li
  checkLabels (tResults tr) li

testEffectPersist :: (Connection c1, Connection c2)
                     => IO c1
                     -> IO c2
                     -> [DeviceId]
                     -> Effect
                     -> (String -> IO ())
                     -> IO ()
testEffectPersist rsrc1 rsrc2 devs defaultEff step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
      eff = defaultEff { eColor = blue
                       , ePeriod = 0.2
                       , ePersist = True
                       , ePowerOn = False
                       }

  step "performing effect"
  effResult <- effect conn1 sels eff
  dly
  threadDelay 300000

  step "listing lights" -- effect should persist
  li <- listLights conn2 sels needEverything
  checkLabels effResult li
  checkColor (zip3 devs (repeat On) (repeat $ makeComplete blue)) li
  checkLabels (tResults tr) li

testEffectFrom :: (Connection c1, Connection c2)
                  => IO c1
                  -> IO c2
                  -> [DeviceId]
                  -> Effect
                  -> (String -> IO ())
                  -> IO ()
testEffectFrom rsrc1 rsrc2 devs defaultEff step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
      eff = defaultEff { eColor = blue
                       , eFromColor = green
                       , ePeriod = 0.2
                       , ePowerOn = False
                       }

  step "performing effect"
  effResult <- effect conn1 sels eff
  dly
  threadDelay 3000000

  step "listing lights" -- effect should have had no lasting... uh, effect
  li <- listLights conn2 sels needEverything
  checkLabels effResult li
  checkColor (zip3 devs (repeat On) (repeat defaultColor)) li
  checkLabels (tResults tr) li

hueColor :: LiFrac -> MaybeColor
hueColor h = HSBK (Just h) (Just 1) Nothing Nothing

testBreatheFromPersist :: (Connection c1, Connection c2)
                          => IO c1
                          -> IO c2
                          -> [DeviceId]
                          -> (String -> IO ())
                          -> IO ()
testBreatheFromPersist rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
      eff = defaultEffect { eType = Breathe
                          , eColor = hueColor 100
                          , eFromColor = hueColor 200
                          , ePeriod = 0.2
                          , eCycles = 1.25
                          , ePersist = True
                          , ePowerOn = False
                          }

  step "performing effect"
  effResult <- effect conn1 sels eff
  dly
  threadDelay 3000000

  step "listing lights"
  li <- listLights conn2 sels [NeedLabel, NeedPower, NeedColor]
  checkLabels effResult li
  -- we expect the breathe to stop halfway between the two colors
  checkColor (zip3 devs (repeat On) (repeat $ makeComplete $ hueColor 150)) li
  checkLabels (tResults tr) li

testBreatheFromPersistHSBK :: (Connection c1, Connection c2)
                              => IO c1
                              -> IO c2
                              -> [DeviceId]
                              -> (String -> IO ())
                              -> IO ()
testBreatheFromPersistHSBK rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs
      eff = defaultEffect { eType = Breathe
                          , eColor = justColor $ HSBK 100 0.4 0.2 3000
                          , eFromColor = justColor $ HSBK 200 0.6 0.4 5000
                          , ePeriod = 0.2
                          , eCycles = 1.25
                          , ePersist = True
                          , ePowerOn = False
                          }

  step "performing effect"
  effResult <- effect conn1 sels eff
  dly
  threadDelay 3000000

  step "listing lights"
  li <- listLights conn2 sels [NeedLabel, NeedPower, NeedColor]
  checkLabels effResult li
  -- we expect the breathe to stop halfway between the two colors
  checkColor (zip3 devs (repeat On) (repeat $ HSBK 150 0.5 0.3 4000)) li
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

-- These expected values were obtained from the Validate Color endpoint:
-- http://api.developer.lifx.com/docs/validate-color
rgbTests :: [(T.Text, Color)]
rgbTests =
  [ ("#ffffff", HSBK 0 0 1 0)
  , ("#000000", HSBK 0 0 0 0)
  , ("rgb:0,0,0", HSBK 0 0 0.0 0)
  , ("rgb:0,0,128", HSBK 240.0 1.0 0.5019607843137255 0)
  , ("rgb:0,0,139", HSBK 240.0 1.0 0.5450980392156862 0)
  , ("rgb:0,0,205", HSBK 240.0 1.0 0.803921568627451 0)
  , ("rgb:0,0,255", HSBK 240.0 1.0 1.0 0)
  , ("rgb:0,100,0", HSBK 120.0 1.0 0.39215686274509803 0)
  , ("rgb:0,128,0", HSBK 120.0 1.0 0.5019607843137255 0)
  , ("rgb:0,128,128", HSBK 180.0 1.0 0.5019607843137255 0)
  , ("rgb:0,139,139", HSBK 180.0 1.0 0.5450980392156862 0)
  , ("rgb:0,191,255", HSBK 195.05882352941177 1.0 1.0 0)
  , ("rgb:0,206,209", HSBK 180.86124401913875 1.0 0.8196078431372549 0)
  , ("rgb:0,250,154", HSBK 156.96 1.0 0.9803921568627451 0)
  , ("rgb:0,255,0", HSBK 120.0 1.0 1.0 0)
  , ("rgb:0,255,127", HSBK 149.88235294117646 1.0 1.0 0)
  , ("rgb:0,255,255", HSBK 180.0 1.0 1.0 0)
  , ("rgb:100,149,237", HSBK 218.54014598540147 0.5780590717299579 0.9294117647058824 0)
  , ("rgb:102,205,170", HSBK 159.61165048543688 0.5024390243902439 0.803921568627451 0)
  , ("rgb:105,105,105", HSBK 0 0.0 0.4117647058823529 0)
  , ("rgb:106,90,205", HSBK 248.34782608695653 0.5609756097560975 0.803921568627451 0)
  , ("rgb:107,142,35", HSBK 79.62616822429906 0.7535211267605634 0.5568627450980392 0)
  , ("rgb:112,128,144", HSBK 210.0 0.2222222222222222 0.5647058823529412 0)
  , ("rgb:119,136,153", HSBK 210.0 0.22222222222222218 0.6 0)
  , ("rgb:123,104,238", HSBK 248.50746268656715 0.5630252100840336 0.9333333333333333 0)
  , ("rgb:124,252,0", HSBK 90.47619047619048 1.0 0.9882352941176471 0)
  , ("rgb:127,255,0", HSBK 90.11764705882354 1.0 1.0 0)
  , ("rgb:127,255,212", HSBK 159.84375 0.5019607843137255 1.0 0)
  , ("rgb:128,0,0", HSBK 0.0 1.0 0.5019607843137255 0)
  , ("rgb:128,0,128", HSBK 300.0 1.0 0.5019607843137255 0)
  , ("rgb:128,128,0", HSBK 60.0 1.0 0.5019607843137255 0)
  , ("rgb:128,128,128", HSBK 0 0.0 0.5019607843137255 0)
  , ("rgb:135,206,235", HSBK 197.4 0.42553191489361697 0.9215686274509803 0)
  , ("rgb:135,206,250", HSBK 202.95652173913044 0.45999999999999996 0.9803921568627451 0)
  , ("rgb:138,43,226", HSBK 271.1475409836066 0.8097345132743362 0.8862745098039215 0)
  , ("rgb:139,0,0", HSBK 0.0 1.0 0.5450980392156862 0)
  , ("rgb:139,0,139", HSBK 300.0 1.0 0.5450980392156862 0)
  , ("rgb:139,69,19", HSBK 24.999999999999996 0.8633093525179856 0.5450980392156862 0)
  , ("rgb:143,188,143", HSBK 120.0 0.23936170212765961 0.7372549019607844 0)
  , ("rgb:144,238,144", HSBK 120.0 0.39495798319327735 0.9333333333333333 0)
  , ("rgb:147,112,219", HSBK 259.6261682242991 0.4885844748858447 0.8588235294117647 0)
  , ("rgb:148,0,211", HSBK 282.08530805687207 1.0 0.8274509803921568 0)
  , ("rgb:152,251,152", HSBK 120.0 0.3944223107569721 0.984313725490196 0)
  , ("rgb:153,50,204", HSBK 280.12987012987014 0.7549019607843138 0.8 0)
  , ("rgb:154,205,50", HSBK 79.74193548387098 0.7560975609756098 0.803921568627451 0)
  , ("rgb:160,82,45", HSBK 19.30434782608696 0.7187499999999999 0.6274509803921569 0)
  , ("rgb:165,42,42", HSBK 0.0 0.7454545454545455 0.6470588235294118 0)
  , ("rgb:169,169,169", HSBK 0 0.0 0.6627450980392157 0)
  , ("rgb:173,216,230", HSBK 194.73684210526315 0.24782608695652172 0.9019607843137255 0)
  , ("rgb:173,255,47", HSBK 83.65384615384615 0.8156862745098039 1.0 0)
  , ("rgb:175,238,238", HSBK 180.0 0.2647058823529412 0.9333333333333333 0)
  , ("rgb:176,196,222", HSBK 213.91304347826087 0.20720720720720723 0.8705882352941177 0)
  , ("rgb:176,224,230", HSBK 186.66666666666666 0.2347826086956522 0.9019607843137255 0)
  , ("rgb:178,34,34", HSBK 0.0 0.8089887640449438 0.6980392156862745 0)
  , ("rgb:184,134,11", HSBK 42.65895953757225 0.9402173913043479 0.7215686274509804 0)
  , ("rgb:186,85,211", HSBK 288.0952380952381 0.5971563981042654 0.8274509803921568 0)
  , ("rgb:188,143,143", HSBK 0.0 0.23936170212765961 0.7372549019607844 0)
  , ("rgb:189,183,107", HSBK 55.609756097560975 0.4338624338624339 0.7411764705882353 0)
  , ("rgb:192,192,192", HSBK 0 0.0 0.7529411764705882 0)
  , ("rgb:199,21,133", HSBK 322.24719101123594 0.8944723618090452 0.7803921568627451 0)
  , ("rgb:205,133,63", HSBK 29.577464788732396 0.6926829268292682 0.803921568627451 0)
  , ("rgb:205,92,92", HSBK 0.0 0.551219512195122 0.803921568627451 0)
  , ("rgb:210,105,30", HSBK 24.999999999999996 0.8571428571428571 0.8235294117647058 0)
  , ("rgb:210,180,140", HSBK 34.2857142857143 0.33333333333333326 0.8235294117647058 0)
  , ("rgb:211,211,211", HSBK 0 0.0 0.8274509803921568 0)
  , ("rgb:216,191,216", HSBK 300.0 0.11574074074074073 0.8470588235294118 0)
  , ("rgb:218,112,214", HSBK 302.2641509433962 0.4862385321100917 0.8549019607843137 0)
  , ("rgb:218,165,32", HSBK 42.903225806451616 0.8532110091743119 0.8549019607843137 0)
  , ("rgb:219,112,147", HSBK 340.3738317757009 0.4885844748858447 0.8588235294117647 0)
  , ("rgb:220,20,60", HSBK 348.0 0.9090909090909092 0.8627450980392157 0)
  , ("rgb:220,220,220", HSBK 0 0.0 0.8627450980392157 0)
  , ("rgb:221,160,221", HSBK 300.0 0.27601809954751133 0.8666666666666667 0)
  , ("rgb:222,184,135", HSBK 33.79310344827586 0.3918918918918919 0.8705882352941177 0)
  , ("rgb:224,255,255", HSBK 180.0 0.1215686274509804 1.0 0)
  , ("rgb:230,230,250", HSBK 240.0 0.07999999999999995 0.9803921568627451 0)
  , ("rgb:233,150,122", HSBK 15.135135135135137 0.4763948497854077 0.9137254901960784 0)
  , ("rgb:238,130,238", HSBK 300.0 0.45378151260504207 0.9333333333333333 0)
  , ("rgb:238,232,170", HSBK 54.70588235294117 0.28571428571428575 0.9333333333333333 0)
  , ("rgb:240,128,128", HSBK 0.0 0.4666666666666667 0.9411764705882353 0)
  , ("rgb:240,230,140", HSBK 54.0 0.41666666666666663 0.9411764705882353 0)
  , ("rgb:240,248,255", HSBK 208.0 0.05882352941176472 1.0 0)
  , ("rgb:240,255,240", HSBK 120.0 0.05882352941176472 1.0 0)
  , ("rgb:240,255,255", HSBK 180.0 0.05882352941176472 1.0 0)
  , ("rgb:244,164,96", HSBK 27.56756756756757 0.6065573770491803 0.9568627450980393 0)
  , ("rgb:245,222,179", HSBK 39.09090909090909 0.2693877551020409 0.9607843137254902 0)
  , ("rgb:245,245,220", HSBK 60.0 0.10204081632653059 0.9607843137254902 0)
  , ("rgb:245,245,245", HSBK 0 0.0 0.9607843137254902 0)
  , ("rgb:245,255,250", HSBK 149.99999999999991 0.039215686274509776 1.0 0)
  , ("rgb:248,248,255", HSBK 240.0 0.027450980392156876 1.0 0)
  , ("rgb:25,25,112", HSBK 240.0 0.7767857142857143 0.4392156862745098 0)
  , ("rgb:250,128,114", HSBK 6.176470588235292 0.5439999999999999 0.9803921568627451 0)
  , ("rgb:250,235,215", HSBK 34.28571428571427 0.13999999999999996 0.9803921568627451 0)
  , ("rgb:250,240,230", HSBK 30.0 0.07999999999999995 0.9803921568627451 0)
  , ("rgb:250,250,210", HSBK 60.0 0.16 0.9803921568627451 0)
  , ("rgb:253,245,230", HSBK 39.130434782608695 0.09090909090909093 0.9921568627450981 0)
  , ("rgb:255,0,0", HSBK 0.0 1.0 1.0 0)
  , ("rgb:255,0,255", HSBK 300.0 1.0 1.0 0)
  , ("rgb:255,105,180", HSBK 330.0 0.5882352941176471 1.0 0)
  , ("rgb:255,127,80", HSBK 16.114285714285714 0.6862745098039216 1.0 0)
  , ("rgb:255,140,0", HSBK 32.94117647058824 1.0 1.0 0)
  , ("rgb:255,160,122", HSBK 17.142857142857142 0.5215686274509803 1.0 0)
  , ("rgb:255,165,0", HSBK 38.82352941176471 1.0 1.0 0)
  , ("rgb:255,182,193", HSBK 350.958904109589 0.28627450980392155 1.0 0)
  , ("rgb:255,192,203", HSBK 349.5238095238096 0.24705882352941178 1.0 0)
  , ("rgb:255,20,147", HSBK 327.5744680851064 0.9215686274509804 1.0 0)
  , ("rgb:255,215,0", HSBK 50.588235294117645 1.0 1.0 0)
  , ("rgb:255,218,185", HSBK 28.285714285714278 0.27450980392156865 1.0 0)
  , ("rgb:255,222,173", HSBK 35.853658536585364 0.32156862745098036 1.0 0)
  , ("rgb:255,228,181", HSBK 38.10810810810811 0.2901960784313725 1.0 0)
  , ("rgb:255,228,196", HSBK 32.54237288135594 0.2313725490196078 1.0 0)
  , ("rgb:255,228,225", HSBK 6.000000000000034 0.11764705882352944 1.0 0)
  , ("rgb:255,235,205", HSBK 35.99999999999998 0.196078431372549 1.0 0)
  , ("rgb:255,239,213", HSBK 37.14285714285714 0.16470588235294115 1.0 0)
  , ("rgb:255,240,245", HSBK 339.99999999999994 0.05882352941176472 1.0 0)
  , ("rgb:255,245,238", HSBK 24.705882352941195 0.06666666666666665 1.0 0)
  , ("rgb:255,248,220", HSBK 47.999999999999986 0.13725490196078427 1.0 0)
  , ("rgb:255,250,205", HSBK 53.999999999999986 0.196078431372549 1.0 0)
  , ("rgb:255,250,240", HSBK 39.999999999999964 0.05882352941176472 1.0 0)
  , ("rgb:255,250,250", HSBK 0.0 0.019607843137254943 1.0 0)
  , ("rgb:255,255,0", HSBK 60.0 1.0 1.0 0)
  , ("rgb:255,255,224", HSBK 60.0 0.1215686274509804 1.0 0)
  , ("rgb:255,255,240", HSBK 60.0 0.05882352941176472 1.0 0)
  , ("rgb:255,255,255", HSBK 0 0.0 1.0 0)
  , ("rgb:255,69,0", HSBK 16.235294117647058 1.0 1.0 0)
  , ("rgb:255,99,71", HSBK 9.130434782608695 0.7215686274509804 1.0 0)
  , ("rgb:30,144,255", HSBK 209.6 0.8823529411764706 1.0 0)
  , ("rgb:32,178,170", HSBK 176.71232876712327 0.8202247191011236 0.6980392156862745 0)
  , ("rgb:34,139,34", HSBK 120.0 0.7553956834532375 0.5450980392156862 0)
  , ("rgb:46,139,87", HSBK 146.45161290322582 0.6690647482014388 0.5450980392156862 0)
  , ("rgb:47,79,79", HSBK 180.0 0.4050632911392405 0.30980392156862746 0)
  , ("rgb:50,205,50", HSBK 120.0 0.7560975609756098 0.803921568627451 0)
  , ("rgb:60,179,113", HSBK 146.72268907563026 0.664804469273743 0.7019607843137254 0)
  , ("rgb:64,224,208", HSBK 174.0 0.7142857142857143 0.8784313725490196 0)
  , ("rgb:65,105,225", HSBK 225.0 0.7111111111111111 0.8823529411764706 0)
  , ("rgb:70,130,180", HSBK 207.27272727272728 0.611111111111111 0.7058823529411765 0)
  , ("rgb:72,209,204", HSBK 177.8102189781022 0.6555023923444976 0.8196078431372549 0)
  , ("rgb:72,61,139", HSBK 248.46153846153848 0.5611510791366905 0.5450980392156862 0)
  , ("rgb:75,0,130", HSBK 274.6153846153846 1.0 0.5098039215686274 0)
  , ("rgb:85,107,47", HSBK 82.0 0.5607476635514018 0.4196078431372549 0)
  , ("rgb:95,158,160", HSBK 181.84615384615384 0.40625 0.6274509803921569 0)
  ]

testRGB :: IO ()
testRGB =
  forM_ rgbTests $ \(txt, expected) -> do
    let actual = zeroColor `combineColors` (fromJust $ parseColor txt)
        zeroColor = justColor $ HSBK 0 0 0 0
    assertColorEqual (T.unpack txt) expected (definitelyColor actual)

colorErrorTests =
  [ tst "rgb:0,0,ff"
  , tst "brown"
  , tst "#ddffgg"
  , tst "#fffff"
  , tst "#fffffff"
  , tst "hue:120 saturation:1.0 brightness:0.5 kelbin:5000"
  , tst "rgb:0x"
  ]
  where tst c = testCase c $ msg $ parseColor (T.pack c)
        msg Nothing = return ()
        msg (Just _ ) = assertFailure "unexpectedly successful"

frt :: LifxId a => T.Text -> a
frt = fromRight . fromText

selectorTest :: IO ()
selectorTest = do
  let txt = "all,label:Banana,id:aabbccddeeff,group:Apple,location:Orange,"
            <> "group_id:00112233445566778899aabbccddeeff,"
            <> "location_id:defacedbadfacadedefacedbadfacade"
      sels = [ SelAll
             , SelLabel      $ frt "Banana"
             , SelDevId      $ frt "aabbccddeeff"
             , SelGroup      $ frt "Apple"
             , SelLocation   $ frt "Orange"
             , SelGroupId    $ frt "00112233445566778899aabbccddeeff"
             , SelLocationId $ frt "defacedbadfacadedefacedbadfacade"
             ]
  assertEqual "parseSelectors" (Just sels) (parseSelectors txt)
  assertEqual "selectorsToText" txt (fromRight' $ selectorsToText sels)

selectorErrorTests =
  [ tst "allow"
  , tst "id:aabbccddeeff00"
  , tst "id:bbccddeeffgg"
  , tst "id:aabbccddeef"
  , tst "group_id:0123456789"
  , tst "group_id:whatever"
  , tst "group_id:"
  , tst "location_id:aaaaaaaaaaaaaaaa"
  , tst "location_id:aaaaaaaaaaaaaaaaa"
  ]
  where tst s = testCase s $ msg $ parseSelector (T.pack s)
        msg Nothing = return ()
        msg (Just _ ) = assertFailure "unexpectedly successful"

selectorsToTextErrorTest :: IO ()
selectorsToTextErrorTest = do
  let sels = [ SelLabel $ frt "Hello, World!" ]
      actual = selectorsToText sels
      expected = Left $ IllegalCharacter ','
  assertEqual "selectorsToTextErrorTest" actual expected

{-# LANGUAGE OverloadedStrings #-}

module HardwareTests (hardwareTests) where

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Char
import Data.Hourglass
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

import Util

hardwareTests =
  let devs = map (fromRight . fromText) ["d073d5029e03", "d073d502b95f"]
  in withResource initCloud closeConnection
    $ \cr -> withResource (initLan devs) closeConnection
             $ \lr -> testGroup "Hardware Tests"
                      [ testGroup "Cloud" (someTests cr cr devs)
                        {-
                      , testGroup "Lan"   (someTests lr lr devs)
                      , testGroup "CloudAndLan" (someTests cr lr devs)
                      , testGroup "LanAndCloud" (someTests lr cr devs)
                        -}
                      ]
  where
    initCloud = do
      lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
      let lifxToken = fromRight $ fromText $ T.pack $ takeWhile (not . isSpace) lifxTokenStr
          cs = defaultCloudSettings { csToken = lifxToken }
      openCloudConnection cs
    initLan devs = do
      lc <- openLanConnection (myLanSettings devs)
      threadDelay 1000000
      return lc

myLanSettings devs =
  defaultLanSettings { lsListScenes = return $ myScenes devs }

longAgo = DateTime d t
  where d = Date 1776 July 4
        t = TimeOfDay (Hours 0) (Minutes 0) (Seconds 0) (NanoSeconds 0)

myScenes devs =
  [ Scene
    { scId = fromRight $ fromText "0c18d0b8-e5c3-11e5-b5ac-0050c2490048"
    , scName = "Hardcoded test scene for LAN"
    , scUpdatedAt = longAgo
    , scCreatedAt = longAgo
    , scAccount = Nothing
    , scStates = zipWith mkState devs (reverse colors)
    }
  ]
  where mkState dev color = SceneState (SelDevId dev) (Just On) color

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
    {-
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
    -}
  , testCaseSteps "activate scene" (testActivateScene conn1 conn2 devs)
  , testCaseSteps "select by scene" (testSelectScene conn1 conn2 devs)
  , testCaseSteps "select by label" (testSelectLabel conn1 conn2 devs)
  , testCase "activate nonexistent scene" (testActivateSceneNonexistent conn1 conn2 devs)
  {-
  , testCase "nonexistent device" (testNonexistentDevice conn1 conn2 devs)
  , testCase "nonexistent label" (testNonexistentLabel conn1 conn2 devs)
  , testCase "nonexistent group id" (testNonexistentGroupId conn1 conn2 devs)
  , testCase "nonexistent group" (testNonexistentGroup conn1 conn2 devs)
  , testCase "nonexistent location id" (testNonexistentLocationId conn1 conn2 devs)
  , testCase "nonexistent location" (testNonexistentLocation conn1 conn2 devs)
  -}
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

findAppropriateScene :: [Scene] -> [DeviceId] -> Maybe Scene
findAppropriateScene scenes devs =
  let selsInScenes = map selsInScene scenes
      selsInScene scene = sort $ map ssSel $ scStates scene
      pairs = zip selsInScenes scenes
  in map SelDevId (sort devs) `lookup` pairs

getAppropriateScene :: Connection c
                       => c
                       -> [DeviceId]
                       -> IO Scene
getAppropriateScene conn devs = do
  scenes <- listScenes conn
  case findAppropriateScene scenes devs of
   Nothing -> assertFailure ("Could not find a scene containing only "
                             ++ show devs) >> undefined
   Just scene -> return scene

testActivateScene :: (Connection c1, Connection c2)
                     => IO c1
                     -> IO c2
                     -> [DeviceId]
                     -> (String -> IO ())
                     -> IO ()
testActivateScene rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs

  step "finding a scene to use"
  scene <- getAppropriateScene conn1 devs

  step $ "activating scene " ++ show (scName scene)
  rs <- activateScene conn1 (scId scene) 0
  dly

  step "listing lights"
  li <- listLights conn2 sels [NeedLabel, NeedPower, NeedColor]
  checkLabels rs li
  checkScenes scene li

testSelectScene :: (Connection c1, Connection c2)
                   => IO c1
                   -> IO c2
                   -> [DeviceId]
                   -> (String -> IO ())
                   -> IO ()
testSelectScene rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  let sels = map SelDevId devs

  step "finding a scene to use"
  scene <- getAppropriateScene conn1 devs

  step $ "setting state, selecting on " ++ show (scName scene)
  result <- setState conn1 [SelSceneId $ scId scene]
            $ StateTransition Nothing yellow 0
  dly

  step "listing lights"
  li <- listLights conn2 sels [NeedLabel, NeedPower, NeedColor]
  checkLabels result li
  checkColor (zip3 devs (repeat On) (repeat $ makeComplete yellow)) li
  checkLabels (tResults tr) li

testSelectLabel :: (Connection c1, Connection c2)
                   => IO c1
                   -> IO c2
                   -> [DeviceId]
                   -> (String -> IO ())
                   -> IO ()
testSelectLabel rsrc1 rsrc2 devs step = do
  (conn1, conn2) <- getConnections rsrc1 rsrc2
  tr <- knownState conn1 devs step
  let labs = map (fromJust . rLabel) (tResults tr)
  let sels = map SelLabel labs

  step "listing lights"
  li <- listLights conn2 sels needEverything
  checkColor (zip3 devs (repeat On) (repeat defaultColor)) li
  checkLabels (tResults tr) li

-- if expected is Nothing, we don't care what actual is
assertConsistent :: Show a
                    => (String -> a -> a -> IO ())
                    -> String
                    -> Maybe a
                    -> Maybe a
                    -> IO ()
assertConsistent _ _ Nothing _ = return ()
assertConsistent f msg (Just expected) (Just actual) = f msg expected actual
assertConsistent _ msg (Just expected) Nothing =
  assertFailure (msg ++ ": expected " ++ show expected ++ " but got Nothing")

assertConsistentEq     = assertConsistent assertEqual
assertConsistentHue    = assertConsistent (assertCloseEnough360 (360 / 500))
assertConsistentSatBri = assertConsistent (assertCloseEnough (1 / 500))
assertConsistentKelvin = assertConsistent (assertCloseEnough 3)

assertConsistentColor :: String -> MaybeColor -> MaybeColor -> IO ()
assertConsistentColor msg expected actual = do
  -- https://community.lifx.com/t/some-weird-observations-when-writing-automated-tests/1080
  assertConsistentHue    (msg ++ ": hue")        (hue expected) (hue actual)
  assertConsistentSatBri (msg ++ ": saturation") (saturation expected) (saturation actual)
  assertConsistentSatBri (msg ++ ": brightness") (brightness expected) (brightness actual)
  assertConsistentKelvin (msg ++ ": kelvin")     (kelvin expected) (kelvin actual)

checkScenes :: Scene -> [LightInfo] -> IO ()
checkScenes scene li = do
  let states = sortBy (comparing ssSel) (scStates scene)
      li' = sortBy (comparing lId) li
      pairs = zip states li'
  forM_ pairs $ \(state, linfo) -> do
    assertEqual "device IDs" (ssSel state) (SelDevId $ lId linfo)
    assertConsistentEq "Power" (ssPower state) (lPower linfo)
    assertConsistentColor "Color" (ssColor state) (lColor linfo)

checkExc :: LifxException -> LifxException -> IO ()
checkExc expected actual = assertEqual "exception" expected actual

expectExc :: IO ()
expectExc = assertFailure "expected an exception, and didn't get one"

testActivateSceneNonexistent :: (Connection c1, Connection c2)
                                => IO c1
                                -> IO c2
                                -> [DeviceId]
                                -> IO ()
testActivateSceneNonexistent rsrc1 rsrc2 _ = do
  (conn1, _ ) <- getConnections rsrc1 rsrc2

  let badScene = fromRight $ fromText $ "55213c0c-e5c9-11e5-80f7-0050c2490048"
  (activateScene conn1 badScene 0 >> expectExc)
    `catch` checkExc (SelectorNotFound $ SelSceneId badScene)
  dly

testNonexistentDevice :: (Connection c1, Connection c2)
                         => IO c1
                         -> IO c2
                         -> [DeviceId]
                         -> IO ()
testNonexistentDevice rsrc1 rsrc2 _ = do
  (conn1, _ ) <- getConnections rsrc1 rsrc2
  let nonDev = fromRight $ fromText "0015edaabbcc"

  (togglePower conn1 [SelDevId nonDev] 1.0 >> expectExc)
    `catch` checkExc (SelectorNotFound $ SelDevId nonDev)
  dly

testNonexistentLabel :: (Connection c1, Connection c2)
                        => IO c1
                        -> IO c2
                        -> [DeviceId]
                        -> IO ()
testNonexistentLabel rsrc1 rsrc2 _ = do
  (conn1, _ ) <- getConnections rsrc1 rsrc2
  let nonLab = fromRight $ fromText "mfgH2fYENVpO1SIu5w5wbmfVuLiqV6Ct"

  (togglePower conn1 [SelLabel nonLab] 1.0 >> expectExc)
    `catch` checkExc (SelectorNotFound $ SelLabel nonLab)
  dly

testNonexistentGroupId :: (Connection c1, Connection c2)
                          => IO c1
                          -> IO c2
                          -> [DeviceId]
                          -> IO ()
testNonexistentGroupId rsrc1 rsrc2 _ = do
  (conn1, _ ) <- getConnections rsrc1 rsrc2
  let nonGrp = fromRight $ fromText "feedfacedeadbeefdefacedbadfacade"

  (togglePower conn1 [SelGroupId nonGrp] 1.0 >> expectExc)
    `catch` checkExc (SelectorNotFound $ SelGroupId nonGrp)
  dly

testNonexistentGroup :: (Connection c1, Connection c2)
                        => IO c1
                        -> IO c2
                        -> [DeviceId]
                        -> IO ()
testNonexistentGroup rsrc1 rsrc2 _ = do
  (conn1, _ ) <- getConnections rsrc1 rsrc2
  let nonGrp = fromRight $ fromText "mfgH2fYENVpO1SIu5w5wbmfVuLiqV6Ct"

  (togglePower conn1 [SelGroup nonGrp] 1.0 >> expectExc)
    `catch` checkExc (SelectorNotFound $ SelGroup nonGrp)
  dly

testNonexistentLocationId :: (Connection c1, Connection c2)
                             => IO c1
                             -> IO c2
                             -> [DeviceId]
                             -> IO ()
testNonexistentLocationId rsrc1 rsrc2 _ = do
  (conn1, _ ) <- getConnections rsrc1 rsrc2
  let nonLoc = fromRight $ fromText "feedfacedeadbeefdefacedbadfacade"

  (togglePower conn1 [SelLocationId nonLoc] 1.0 >> expectExc)
    `catch` checkExc (SelectorNotFound $ SelLocationId nonLoc)
  dly

testNonexistentLocation :: (Connection c1, Connection c2)
                           => IO c1
                           -> IO c2
                           -> [DeviceId]
                           -> IO ()
testNonexistentLocation rsrc1 rsrc2 _ = do
  (conn1, _ ) <- getConnections rsrc1 rsrc2
  let nonLoc = fromRight $ fromText "mfgH2fYENVpO1SIu5w5wbmfVuLiqV6Ct"

  (togglePower conn1 [SelLocation nonLoc] 1.0 >> expectExc)
    `catch` checkExc (SelectorNotFound $ SelLocation nonLoc)
  dly

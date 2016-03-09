{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
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

import Util
import PureTests

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
  let devs = map (fromRight . fromText) ["d073d5029e03", "d073d502b95f"]

  defaultMain $ testGroup "All Tests"
    [ pureTests
    , withResource initCloud closeConnection
      $ \cr -> withResource initLan closeConnection
               $ \lr -> testGroup "Hardware Tests"
                        [ testGroup "Cloud" (someTests cr cr devs)
                        , testGroup "Lan"   (someTests lr lr devs)
                        , testGroup "CloudAndLan" (someTests cr lr devs)
                        , testGroup "LanAndCloud" (someTests lr cr devs)
                        ]
    ]

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

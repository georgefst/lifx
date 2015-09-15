{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad ( when, forever, forM_, unless )
import qualified Control.Exception as E (catch, throw, AsyncException(..))
import Data.Bits
import qualified Data.ByteString as B
import Data.Char
import Data.Hourglass
import Data.Int ( Int64 )
import Data.List hiding (insert)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Set hiding (map, filter)
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Text.Format
import Data.Text.Format.Params
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as TIO
import Data.Word ( Word16, Word32, Word64 )
import GHC.Float
import qualified Network.Info as NI
import System.Console.CmdArgs.Explicit
import System.Exit
import System.Hourglass

import Lifx.Lan.LowLevel
import Lifx hiding (Pulse)
import qualified Lifx.Program.CmdParser as C
import Lifx.Program.Column

{-
myTime :: Word64 -> String
myTime nanos =
  let (q, r) = quotRem nanos 1000000000
      elapp = ElapsedP (fromIntegral q) (fromIntegral r)
  in timePrint ISO8601_DateAndTime elapp

myCb :: Bulb -> IO ()
myCb bulb = do
  print bulb
  getHostInfo bulb $ \shi -> do
    print shi
    getLight bulb $ \sl -> do
      print sl
      setPower bulb True 1000 $
        setColor bulb (HSBK 32768 65535 65535 3000) 0 $ do
          -- positive numbers mean more time spent on original color
          let swf = SetWaveform False (HSBK 0 0 65535 9000) 1000 10 (-20000) Pulse
          setWaveform bulb swf $
            getHostFirmware bulb $ \shf -> do
              print shf
              let vHex = printf "%x" (shfVersion shf)
              putStrLn $ "build " ++ myTime (shfBuild shf) ++ ", version " ++ vHex
              getWifiFirmware bulb $ \swf -> do
                print swf
                let vHex' = printf "%x" (swfVersion swf)
                putStrLn $ "build " ++ myTime (swfBuild swf) ++ ", version " ++ vHex'
                getVersion bulb $ \sv -> do
                  print sv
                  printf "%x %x %x\n" (svVendor sv) (svProduct sv) (svVersion sv)
                  getInfo bulb $ \si -> do
                    print si
                    putStrLn $ "current time = " ++ myTime (siTime si)
                    print $ nsToDuration $ fromIntegral $ siUptime si
                    print $ nsToDuration $ fromIntegral $ siDowntime si
                    putStrLn "done!"

deriving instance Show Duration
deriving instance Read TimeOfDay
-}

nsToDuration :: NanoSeconds -> (Int64, Duration)
nsToDuration (NanoSeconds ns) =
  (days, Duration (Hours hours) (Minutes minutes)
                  (Seconds seconds) (NanoSeconds nanos))
  where (s, nanos) = ns `quotRem` 1000000000
        (m, seconds) = s `quotRem` 60
        (h, minutes) = m `quotRem` 60
        (days, hours) = h `quotRem` 24

fmt :: Params ps => Format -> ps -> T.Text
fmt f p = LT.toStrict $ format f p

l2 = left 2 ' '
l3 = left 3 ' '
l4 = left 4 ' '
l5 = left 5 ' '

prLight :: StateLight -> (T.Text, T.Text, T.Text) -- label, power, color
prLight sl = (label, power, color)
  where label = toText $ slLabel sl
        power = if slPower sl == On then "On" else "Off"
        color = fmt "{} {} {} {}K" (l3 h, l3 s, l3 b, l4 k)
        hsbk = slColor sl
        h = scale (hue hsbk) 360
        s = scale (saturation hsbk) 100
        b = scale (brightness hsbk) 100
        k = kelvin hsbk
        m = maxBound :: Word16
        asDbl x = fromIntegral x :: Double
        scale x mul = (round $ asDbl x * asDbl mul / asDbl m) :: Int

prHostInfo :: StateHostInfo -> [T.Text] -- temp
prHostInfo shi = [ fmt "{}°C" (Only $ l2 $ fixed 0 temp)
                 , fmt "{}°C" (Only $ l4 $ fixed 1 temp)
                 , fmt "{}°C" (Only $ l5 $ fixed 2 temp)
                 ]
  where temp = (fromIntegral (shiMcuTemperature shi) / 100) :: Double

prInfo :: StateInfo -> [T.Text] -- uptime
prInfo si = fmtDur $ nsToDuration $ fromIntegral $ siUptime si
  where fmtDur (days, Duration (Hours hours) (Minutes minutes)
                      (Seconds seconds) _) =
          let xs = [(days, 'd'), (hours, 'h'), (minutes, 'm'), (seconds, 's')]
              xs' = dropWhile (\(n, _ ) -> n == 0) xs
              txts = map (\(n, s) -> fmt "{}{}" (n, s)) xs'
              choices = tail $ inits txts
          in map mconcat choices

prHostFirmware :: StateHostFirmware -> T.Text -- firmware version
prHostFirmware shf = fmt "{}.{}" (major, minor)
  where v = shfVersion shf
        major = v `shiftR` 16
        minor = v .&. 0xffff

prVersion :: StateVersion -> [T.Text] -- hardware version
prVersion sv = addVers $ f $ productFromId vend prod
  where vend = svVendor sv
        prod = svProduct sv
        vers = svVersion sv
        f (Just p) = pShortName p
        f Nothing = fmt "{}:{}" (vend, prod)
        addVers txt = [txt, fmt "{}v{}" (txt, vers)]

tr :: T.Text -> IO ()
tr = TIO.putStrLn . T.stripEnd

addStateLight :: LiteIds -> StateLight -> LiteIds
addStateLight li sl = li { liLabel = Just (slLabel sl) }

addStateGroup :: LiteIds -> StateGroup -> LiteIds
addStateGroup li sg =
  li { liGroupId = Just (sgGroup sg) , liGroup = Just (sgLabel sg) }

addStateLocation :: LiteIds -> StateLocation -> LiteIds
addStateLocation li slo =
  li { liLocId = Just (sloLocation slo) , liLoc = Just (sloLabel slo) }

data LightRow =
  LightRow
  { lrLabel    :: [T.Text]
  , lrPower    :: [T.Text]
  , lrColor    :: [T.Text]
  , lrTemp     :: [T.Text]
  , lrUptime   :: [T.Text]
  , lrDevId    :: [T.Text]
  , lrFirmware :: [T.Text]
  , lrHardware :: [T.Text]
  , lrGroup    :: [T.Text]
  , lrLocation :: [T.Text]
  }

columns =
  [ Column Lft Lft 16 32  40 ["Label"]               lrLabel
  , Column Lft Lft  3  3   0 ["Pwr", "Power"]        lrPower
  , Column Lft Lft 17 17   0 ["Color"]               lrColor
  , Column Lft Lft  6  7  30 ["Temp", "Temperature"] lrTemp
  , Column Rgt Lft 11 11  90 ["Uptime"]              lrUptime
  , Column Lft Rgt  6 12 100 ["DevID", "Device ID"]  lrDevId
  , Column Lft Lft  3  3   0 ["FW", "Firmware"]      lrFirmware
  , Column Lft Lft  5  7  50 ["HW", "Hardware"]      lrHardware
  , Column Lft Lft  8 32  36 ["Group"]               lrGroup
  , Column Lft Lft  8 32  34 ["Location"]            lrLocation
  ]

fixedCols w = fixColumns w columns

prRow :: Int -> LightRow -> T.Text
prRow w = displayRow' $ fixedCols w

mkRow :: Bulb
         -> StateHostInfo
         -> StateLight
         -> StateHostFirmware
         -> StateVersion
         -> StateInfo
         -> StateGroup
         -> StateLocation
         -> LightRow
mkRow bulb shi sl shf sv si sg slo =
  LightRow [label] [power] [color] temp uptime [devid] [fw] vers [group] [loc]
  where (label, power, color) = prLight sl
        temp = prHostInfo shi
        uptime = prInfo si
        devid = toText $ deviceId bulb
        fw = prHostFirmware shf
        vers = prVersion sv
        group = toText $ sgLabel sg
        loc = toText $ sloLabel slo

type DevID = DeviceId

myQuery :: TSem -> Bulb -> String -> ((a -> IO ()) -> IO ()) -> (a -> IO ()) -> IO ()
myQuery sem bulb op q cb =
  reliableQuery defaultRetryParams q cb $ do
    putStrLn $ show bulb ++ " not responding to " ++ op
    atomically $ signalTSem sem

myAction :: TSem -> Bulb -> String -> (IO () -> IO ()) -> IO () -> IO ()
myAction sem bulb op q cb =
  reliableAction defaultRetryParams q cb $ do
    putStrLn $ show bulb ++ " not responding to " ++ op
    atomically $ signalTSem sem

cmdList :: Int -> TSem -> Bulb -> IO ()
cmdList w sem bulb = do
  let rq = myQuery sem bulb
  rq "getHostInfo" (getHostInfo bulb) $ \shi ->
    rq "getLight" (getLight bulb) $ \sl ->
      rq "getHostFirmware" (getHostFirmware bulb) $ \shf ->
        rq "getVersion" (getVersion bulb) $ \sv ->
          rq "getInfo" (getInfo bulb) $ \si ->
            rq "getGroup" (getGroup bulb) $ \sg ->
              rq "getLocation" (getLocation bulb) $ \slo -> do
                tr $ prRow w $ mkRow bulb shi sl shf sv si sg slo
                atomically $ signalTSem sem

f2ms :: LiFrac -> Word32
f2ms x = round $ 1000 * x

cmdPower :: Power -> LiFrac -> TSem -> Bulb -> IO ()
cmdPower pwr dur sem bulb = do
  let ra = myAction sem bulb
  ra "setPower" (setPower bulb pwr $ f2ms dur) $ atomically $ signalTSem sem

justColor :: Color -> MaybeColor
justColor = fmap Just

definitelyColor :: MaybeColor -> Color
definitelyColor = fmap fromJust

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


cmdColor :: MaybeColor -> LiFrac -> TSem -> Bulb -> IO ()
cmdColor ca dur sem bulb = do
  let rq = myQuery sem bulb
  if isCompleteColor ca
    then setColor' ca
    else rq "getLight" (getLight bulb) $ \sl -> do
    let orig = justColor $ color16toFrac $ slColor sl
        newC = orig `combineColors2` ca
    setColor' newC
  where
    ra = myAction sem bulb
    setColor' newC = ra "setColor" (setColor bulb (colorFracTo16 $ definitelyColor newC) $ f2ms dur) (atomically $ signalTSem sem)

cmdWave :: Waveform -> MaybeColor -> C.PulseArg -> TSem -> Bulb -> IO ()
cmdWave wf ca pa sem bulb = do
  let rq = myQuery sem bulb
  if isCompleteColor ca
    then setColor' ca
    else rq "getLight" (getLight bulb) $ \sl -> do
    let orig = justColor $ color16toFrac $ slColor sl
        newC = orig `combineColors2` ca
    setColor' newC
  where
    ra = myAction sem bulb
    dur = 1000
    setColor' newC =
      let swf = SetWaveform { swTransient = not $ C.paPersist pa
                            , swColor = colorFracTo16 $ definitelyColor newC
                            , swPeriod = round $ 1000 * C.paPeriod pa
                            , swCycles = double2Float $ C.paCycles pa
                            , swDutyCycle = floor $ 65535 * (C.paPeak pa - 0.5)
                            , swWaveform = wf
                            }
      in ra "setWaveform" (setWaveform bulb swf) (atomically $ signalTSem sem)

-- as long as c2 is not empty, acts like combineColors
-- if c2 is empty, find a color "opposite" c1
combineColors2 :: MaybeColor -> MaybeColor -> MaybeColor
combineColors2 c1 c2
  | isEmptyColor c2 = c1 `combineColors` contrastingColor
  | otherwise = c1 `combineColors` c2
  where contrastingColor =
          HSBK { hue = contrastingHue $ hue c1
               , saturation = Just 1
               , brightness = Just 1
               , kelvin = Nothing
               }
        contrastingHue Nothing = Nothing
        contrastingHue (Just h) =
          Just $ fromIntegral $ (round h + 180) `rem` 360

cmdSetLabel :: T.Text -> TSem -> Bulb -> IO ()
cmdSetLabel txt sem bulb = do
  let lbl = either error id $ fromText txt
      ra = myAction sem bulb
  ra "setLabel" (setLabel bulb lbl) (atomically $ signalTSem sem)

forkIO_ f = do
  forkIO f
  return ()

data PingStats =
  PingStats
  { psMin :: !Double
  , psMax :: !Double
  , psSum :: !Double
  , psTx  :: !Int
  , psRx  :: !Int
  }

emptyPS = PingStats
  { psMin = read "Infinity"
  , psMax = 0
  , psSum = 0
  , psTx = 0
  , psRx = 0
  }

incTx :: PingStats -> PingStats
incTx ps = ps { psTx = 1 + psTx ps }

adjPS :: Double -> PingStats -> PingStats
adjPS ms ps@(PingStats mn mx su _ rx) =
  ps { psMin = min mn ms
     , psMax = max mx ms
     , psSum = su + ms
     , psRx = rx + 1
     }

cmdPing :: TVar (M.Map Bulb PingStats) -> Bulb -> IO ()
cmdPing pingMap bulb = forkIO_ $ do
  let payload = B.pack [0..63]
  atomically $ do
    pm <- readTVar pingMap
    let pm' = M.insert bulb emptyPS pm
    writeTVar pingMap pm'
  forM_ [0..] $ \i -> do
    start <- timeCurrentP
    atomically $ do
      pm <- readTVar pingMap
      let pm' = M.adjust incTx bulb pm
      writeTVar pingMap pm'
    echoRequest bulb payload $ \_ -> do
      finish <- timeCurrentP
      let (Seconds s, NanoSeconds ns) = finish `timeDiffP` start
          ms = fromIntegral ns / 1000000 + fromIntegral s * 1000
      TIO.putStrLn $ fmt "({}) {} {}ms"
        ( Shown bulb
        , left 4 ' ' $ '#' : show (i :: Integer)
        , left 8 ' ' $ fixed 3 ms )
      atomically $ do
        pm <- readTVar pingMap
        let pm' = M.adjust (adjPS ms) bulb pm
        writeTVar pingMap pm'
    threadDelay 1000000 -- 1 second

cmd2func :: C.LiteCmd -> LiFrac -> TSem -> Bulb -> IO ()
cmd2func (C.CmdList w) _ = cmdList w
cmd2func C.CmdOn dur = cmdPower On dur
cmd2func C.CmdOff dur = cmdPower Off dur
cmd2func (C.CmdColor ca) dur = cmdColor ca dur
cmd2func (C.CmdPulse ca pa) _ = cmdWave Pulse ca pa
cmd2func (C.CmdBreathe ca pa) _ = cmdWave Sine ca pa
cmd2func (C.CmdSetLabel lbl) _ = cmdSetLabel lbl

lsHeader :: Int -> IO ()
lsHeader w = do
  tr $ displayHeader $ fixedCols w
  tr $ displaySep    $ fixedCols w

hdrIfNeeded :: C.LiteCmd -> IO ()
hdrIfNeeded (C.CmdList w) = lsHeader w
hdrIfNeeded _ = return ()

data NeedQuery = NeedLight | NeedGroup | NeedLoc | NeedNothing
                 deriving (Eq, Ord, Show)

needQuery :: TargetMatch -> NeedQuery
needQuery (TmLabel      _ ) = NeedLight
needQuery (TmDevId      _ ) = NeedNothing
needQuery (TmGroup      _ ) = NeedGroup
needQuery (TmGroupId    _ ) = NeedGroup
needQuery (TmLocation   _ ) = NeedLoc
needQuery (TmLocationId _ ) = NeedLoc

needQueries :: Targets -> S.Set NeedQuery
needQueries TargAll = S.empty
needQueries (TargSome s) = S.map needQuery s

foo :: ((a -> IO ()) -> IO ())
       -> NeedQuery
       -> S.Set NeedQuery
       -> LiteIds
       -> (LiteIds -> a -> LiteIds)
       -> (LiteIds -> IO ())
       -> IO ()
foo query supplies needed lids updLids cb
  | supplies `S.member` needed = query $ \response -> cb $ updLids lids response
  | otherwise = cb lids

countCb :: TVar Integer -> (Bulb -> IO ()) -> Bulb -> IO ()
countCb counter realCb bulb = do
  atomically $ modifyTVar' counter (+ 1)
  realCb bulb

-- only call realCb if bulb is in targs
filterCb :: Targets -> (Bulb -> IO ()) -> Bulb -> IO ()
filterCb targs realCb bulb = do
  let lids1 = mkLiteIds (deviceId bulb)
      needed = needQueries targs
  foo (getLight bulb) NeedLight needed lids1 addStateLight $ \lids2 ->
    foo (getGroup bulb) NeedGroup needed lids2 addStateGroup $ \lids3 ->
    foo (getLocation bulb) NeedLoc needed lids3 addStateLocation $ \lids4 ->
    when (satisfied targs lids4) (realCb bulb)
  where
    satisfied TargAll _ = True
    satisfied (TargSome s) lids = True `S.member` S.map (`tmatch` lids) s

discCb :: TVar (Set DevID) -> (Bulb -> IO ()) -> Bulb -> IO ()
discCb done realCb bulb = do
  let dev = deviceId bulb
  dup <- atomically $ do
    s <- readTVar done
    let d = dev `member` s
    unless d $ writeTVar done $ dev `insert` s
    return d
  unless dup $ realCb bulb

ifaceColumns =
  [ Column Lft Lft  4 10 50 ["Iface", "Interface"] (\n -> [T.pack $ NI.name n])
  , Column Lft Lft 15 15  0 ["IPv4"]       (\n -> [T.pack $ show $ NI.ipv4 n])
  , Column Lft Lft 17 17  0 ["MAC"]        (\n -> [T.pack $ show $ NI.mac n])
  ]

ifaceFixedCols = fixColumns 80 ifaceColumns

fmtIfaceRow :: NI.NetworkInterface -> T.Text
fmtIfaceRow = displayRow' ifaceFixedCols

nonZeroIpv4 :: NI.NetworkInterface -> Bool
nonZeroIpv4 ni = 0 /= getv4 (NI.ipv4 ni)
  where getv4 (NI.IPv4 v4) = v4

fmtInterfaces :: [NI.NetworkInterface] -> T.Text
fmtInterfaces ifaces =
  let rows = map fmtIfaceRow $ filter nonZeroIpv4 ifaces
      rows' = displayHeader ifaceFixedCols : displaySep ifaceFixedCols : rows
  in T.unlines rows'

prInterfaces :: IO ()
prInterfaces = do
  ifaces <- NI.getNetworkInterfaces
  TIO.putStr $ fmtInterfaces ifaces

prLifxException :: LifxException -> IO a
prLifxException (NoSuchInterface ifname _ ) = do
  TIO.putStrLn $ fmt "No such interface \"{}\".  Try instead:" (Only ifname)
  TIO.putStrLn T.empty
  prInterfaces
  exitFailure

pingStatsTxt :: Bulb -> PingStats -> T.Text
pingStatsTxt bulb (PingStats mn mx su tx rx) =
  T.unlines
  [ fmt "--- {} ping statistics ---" (Only $ Shown bulb)
  , fmt "{} packets transmitted, {} packets received, {}% packet loss"
    (tx, rx, fixed 1 $ 100 * (ftx - frx) / ftx)
  , fmt "round-trip min/avg/max = {}/{}/{} ms"
    (fixed 3 mn, fixed 3 (su / frx), fixed 3 mx)
  ]
  where ftx = fromIntegral tx
        frx = fromIntegral rx

handleControlC :: TVar (M.Map Bulb PingStats) -> E.AsyncException -> IO a
handleControlC pingMap E.UserInterrupt = do
  pm <- atomically $ readTVar pingMap
  putStrLn "" -- newline after "^C"
  forM_ (M.toAscList pm) $ \(k, v) -> TIO.putStr (pingStatsTxt k v)
  exitSuccess
handleControlC _ somethingElse = E.throw somethingElse

main = do
  args <- C.parseCmdLine
  let ifname = fromMaybe (T.pack "en1") $ C.aInterface args
      cmd = C.aCmd args
  lan <- openLan ifname `E.catch` prLifxException
  moreMain cmd lan args

moreMain C.CmdPing lan args = do
  s <- newTVarIO S.empty
  pingMap <- newTVarIO M.empty
  discoverBulbs lan $ discCb s $ filterCb (C.aTarget args) (cmdPing pingMap)
  forever (threadDelay 900000000) `E.catch` handleControlC pingMap

moreMain cmd lan args = do
  let func = cmd2func cmd (C.aDuration args)
  hdrIfNeeded cmd
  s <- newTVarIO S.empty
  sem <- atomically $ newTSem 0
  counter <- newTVarIO 0
  forM_ [1..10] $ \_ -> do
    discoverBulbs lan (discCb s $ filterCb (C.aTarget args)
                                  (countCb counter $ func sem))
    threadDelay 100000
  atomically $ do
    c <- readTVar counter
    forM_ [1..c] $ \_ -> waitTSem sem

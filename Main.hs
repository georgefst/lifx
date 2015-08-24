{-# LANGUAGE StandaloneDeriving, NoMonomorphismRestriction, OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad ( when, forever )
import Data.Bits
import Data.Char
import Data.Hourglass
import Data.Int ( Int64 )
import Data.List
import Data.Maybe
import Data.Monoid
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
import qualified STMContainers.Set as STMSet
import System.Console.CmdArgs.Explicit

import Lifx.Lan.LowLevel
import Lifx.Types
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
-}

deriving instance Show Duration

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
  where label = slLabel sl
        power = if slPower sl == 0 then "Off" else "On"
        color = fmt "{} {} {} {}K" (l3 h, l3 s, l3 b, l4 k)
        hsbk = slColor sl
        h = scale (hue hsbk) 360
        s = scale (saturation hsbk) 100
        b = scale (brightness hsbk) 100
        k = kelvin hsbk
        m = maxBound :: Word16
        asInt x = fromIntegral x :: Int
        scale x mul = asInt x * asInt mul `div` asInt m

prHostInfo :: StateHostInfo -> [T.Text] -- temp
prHostInfo shi = [ fmt "{}°C" (Only $ l2 $ fixed 0 temp)
                 , fmt "{}°C" (Only $ l4 $ fixed 1 temp)
                 , fmt "{}°C" (Only $ l5 $ fixed 2 temp)
                 ]
  where temp = ((fromIntegral $ shiMcuTemperature shi) / 100) :: Double

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

prVersion :: StateVersion -> T.Text -- hardware version
prVersion sv = f $ productFromId vend prod
  where vend = svVendor sv
        prod = svProduct sv
        f (Just p) = pShortName p
        f Nothing = fmt "{}:{}" (vend, prod)

tr :: T.Text -> IO ()
tr = TIO.putStrLn . T.stripEnd

columns =
  [ Column Lft Lft 16 32  40 ["Label"]
  , Column Lft Lft  3  3   0 ["Pwr", "Power"]
  , Column Lft Lft 17 17   0 ["Color"]
  , Column Lft Lft  6  7  30 ["Temp", "Temperature"]
  , Column Rgt Lft 11 11  90 ["Uptime"]
  , Column Lft Rgt  6 12 100 ["DevID", "Device ID"]
  , Column Lft Lft  3  3   0 ["FW", "Firmware"]
  , Column Lft Lft  5  7  50 ["HW", "Hardware"]
  ]

fixedCols = fixColumns 80 columns

prBulb :: Bulb
          -> StateHostInfo
          -> StateLight
          -> StateHostFirmware
          -> StateVersion
          -> StateInfo
          -> T.Text
prBulb bulb shi sl shf sv si =
  displayRow fixedCols [[label], [power], [color], temp,
                        uptime, [devid], [fw], [vers]]
  where (label, power, color) = prLight sl
        temp = prHostInfo shi
        uptime = prInfo si
        devid = toText $ deviceId bulb
        fw = prHostFirmware shf
        vers = prVersion sv

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

cmdList :: TSem -> Bulb -> IO ()
cmdList sem bulb = do
  let rq = myQuery sem bulb
  rq "getHostInfo" (getHostInfo bulb) $ \shi ->
    rq "getLight" (getLight bulb) $ \sl ->
      rq "getHostFirmware" (getHostFirmware bulb) $ \shf ->
        rq "getVersion" (getVersion bulb) $ \sv ->
          rq "getInfo" (getInfo bulb) $ \si -> do
            tr $ prBulb bulb shi sl shf sv si
            atomically $ signalTSem sem

f2ms :: LiFrac -> Word32
f2ms x = round $ 1000 * x

cmdPower :: Bool -> LiFrac -> TSem -> Bulb -> IO ()
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


cmdColor :: ColorArg -> LiFrac -> TSem -> Bulb -> IO ()
cmdColor ca dur sem bulb = do
  let rq = myQuery sem bulb
  if isCompleteColor ca
    then setColor' $ customColor ca -- FIXME: handle named colors
    else rq "getLight" (getLight bulb) $ \sl -> do
    let orig = justColor $ color16toFrac $ slColor sl
        newC = orig `combineColors` customColor ca
    setColor' newC
  where
    ra = myAction sem bulb
    setColor' newC = ra "setColor" (setColor bulb (colorFracTo16 $ definitelyColor newC) $ f2ms dur) (atomically $ signalTSem sem)

cmdWave :: Waveform -> C.PulseArg -> TSem -> Bulb -> IO ()
cmdWave wf pa sem bulb = do
  let rq = myQuery sem bulb
      ca = C.paColor pa
  if isCompleteColor ca
    then setColor' $ customColor ca -- FIXME: handle named colors
    else rq "getLight" (getLight bulb) $ \sl -> do
    let orig = justColor $ color16toFrac $ slColor sl
        newC = orig `combineColors` customColor ca
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

cmd2func :: C.LiteCmd -> LiFrac -> TSem -> Bulb -> IO ()
cmd2func C.CmdList _ = cmdList
cmd2func C.CmdOn dur = cmdPower True dur
cmd2func C.CmdOff dur = cmdPower False dur
cmd2func (C.CmdColor ca) dur = cmdColor ca dur
cmd2func (C.CmdPulse pa) _ = cmdWave Pulse pa
cmd2func (C.CmdBreathe pa) _ = cmdWave Sine pa

lsHeader :: IO ()
lsHeader = do
  tr $ displayHeader fixedCols
  tr $ displaySep fixedCols

hdrIfNeeded :: C.LiteCmd -> IO ()
hdrIfNeeded C.CmdList = lsHeader
hdrIfNeeded _ = return ()

discCb :: STMSet.Set DevID -> (Bulb -> IO ()) -> Bulb -> IO ()
discCb done realCb bulb = do
  let dev = deviceId bulb
  dup <- atomically $ do
    d <- STMSet.lookup dev done
    when (not d) $ STMSet.insert dev done
    return d
  when (not dup) $ realCb bulb

main = do
  args <- C.parseCmdLine
  let ifname = fromMaybe (T.pack "en1") $ C.aInterface args
      cmd = C.aCmd args
      func = cmd2func cmd (C.aDuration args)
  lan <- openLan ifname
  hdrIfNeeded cmd
  s <- STMSet.newIO
  sem <- atomically $ newTSem 0
  forever $ do
    discoverBulbs lan (discCb s $ func sem)
    threadDelay 500000

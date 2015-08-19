{-# LANGUAGE StandaloneDeriving #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad ( when, forever )
import Data.Bits
import Data.Char
import Data.Hourglass
{-
    ( ElapsedP(ElapsedP),
      ISO8601_DateAndTime(ISO8601_DateAndTime),
      timePrint )
-}
import Data.Int ( Int64 )
import Data.List
import Data.Maybe
-- import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Word ( Word16, Word32, Word64 )
import qualified STMContainers.Set as STMSet
import System.Console.CmdArgs.Explicit
import Text.Printf ( printf )

import Lifx.Lan.LowLevel
import Lifx.Program.Types hiding (HSBK(..))
import qualified Lifx.Program.Types as TY (HSBK(..))
import qualified Lifx.Program.CmdParser as C

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
      setPower bulb True $
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

nsToDuration :: NanoSeconds -> (Int64, Duration)
nsToDuration (NanoSeconds ns) =
  (days, Duration (Hours hours) (Minutes minutes)
                  (Seconds seconds) (NanoSeconds nanos))
  where (s, nanos) = ns `quotRem` 1000000000
        (m, seconds) = s `quotRem` 60
        (h, minutes) = m `quotRem` 60
        (days, hours) = h `quotRem` 24

prLight :: StateLight -> (String, String, String) -- label, power, color
prLight sl = (label, power, color)
  where label = T.unpack $ TE.decodeUtf8With TEE.lenientDecode $ slLabel sl
        power = if slPower sl == 0 then "Off" else "On"
        color = printf "%3d %3d %3d %4dK" h s b k
        hsbk = slColor sl
        h = scale (hue hsbk) 360
        s = scale (saturation hsbk) 100
        b = scale (brightness hsbk) 100
        k = kelvin hsbk
        m = maxBound :: Word16
        asInt x = fromIntegral x :: Int
        scale x mul = asInt x * asInt mul `div` asInt m

prHostInfo :: StateHostInfo -> String -- temp
prHostInfo shi = printf "%4.1f°C" (temp :: Double)
  where temp = (fromIntegral $ shiMcuTemperature shi) / 100

prInfo :: StateInfo -> String -- uptime
prInfo si = fmtDur $ nsToDuration $ fromIntegral $ siUptime si
  where fmtDur (days, Duration (Hours hours) (Minutes minutes)
                      (Seconds seconds) _) =
          let xs = [(days, "d"), (hours, "h"), (minutes, "m"), (seconds, "s")]
              xs' = dropWhile (\(n, _ ) -> n == 0) xs
          in concatMap (\(n, s) -> show n ++ s) xs'

prHostFirmware :: StateHostFirmware -> String -- firmware version
prHostFirmware shf = printf "%d.%d" major minor
  where v = shfVersion shf
        major = v `shiftR` 16
        minor = v .&. 0xffff

prVersion :: StateVersion -> String -- hardware version
prVersion sv = f $ productFromId vend prod
  where vend = svVendor sv
        prod = svProduct sv
        f (Just p) = T.unpack $ pShortName p
        f Nothing = printf "%d:%d" vend prod

tr :: String -> IO ()
tr s = putStrLn $ dropWhileEnd isSpace s

fmtStr = "%-16.16s %-3.3s %-17.17s %-6.6s %11.11s %-12.12s %-3.3s %-5.5s"

prBulb :: Bulb
          -> StateHostInfo
          -> StateLight
          -> StateHostFirmware
          -> StateVersion
          -> StateInfo
          -> String
prBulb bulb shi sl shf sv si =
  printf fmtStr label power color temp uptime devid fw vers
  where (label, power, color) = prLight sl
        temp = prHostInfo shi
        uptime = prInfo si
        devid = deviceId bulb
        fw = prHostFirmware shf
        vers = prVersion sv

type DevID = String

-- ra q cb = reliableAction defaultRetryParams q cb $ return ()
rq q cb = reliableQuery  defaultRetryParams q cb $ putStrLn "timeout!"

lsCb :: STMSet.Set DevID -> Bulb -> IO ()
lsCb done bulb = do
  let dev = deviceId bulb
  already <- atomically $ STMSet.lookup dev done
  when (not already) $
    rq (getHostInfo bulb) $ \shi ->
      rq (getLight bulb) $ \sl ->
        rq (getHostFirmware bulb) $ \shf ->
          rq (getVersion bulb) $ \sv ->
            rq (getInfo bulb) $ \si -> do
              dup <- atomically $ do
                d <- STMSet.lookup dev done
                when (not d) $ STMSet.insert dev done
                return d
              when (not dup) $ tr $ prBulb bulb shi sl shf sv si

lsBulbs :: Lan -> IO ()
lsBulbs lan = do
  let fmtStrn = fmtStr ++ "\n"
      dashes = replicate 80 '-'
  tr $ printf fmtStrn "Label" "Pwr" "Color" "Temp" "Uptime" "DevID" "FW" "HW"
  tr $ printf fmtStrn dashes dashes dashes dashes dashes dashes dashes dashes
  s <- STMSet.newIO
  forever $ do
    discoverBulbs lan (lsCb s)
    threadDelay 500000

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

cmdPower :: Bool -> TSem -> Bulb -> IO ()
cmdPower pwr sem bulb = do
  let ra = myAction sem bulb
  ra "setPower" (setPower bulb pwr) $ atomically $ signalTSem sem

justColor :: Color -> MaybeColor
justColor = fmap Just

definitelyColor :: MaybeColor -> Color
definitelyColor = fmap fromJust

color16toFrac :: HSBK -> Color
color16toFrac c = TY.HSBK
  { TY.hue = fromIntegral (hue c) / 65535 * 360
  , TY.saturation = fromIntegral (saturation c) / 65535
  , TY.brightness = fromIntegral (brightness c) / 65535
  , TY.kelvin = fromIntegral (kelvin c)
  }

colorFracTo16 :: Color -> HSBK
colorFracTo16 c = HSBK
  { hue = round $ TY.hue c * 65535 / 360
  , saturation = round $ TY.saturation c * 65535
  , brightness = round $ TY.brightness c * 65535
  , kelvin = round $ TY.kelvin c
  }


cmdColor :: ColorArg -> TSem -> Bulb -> IO ()
cmdColor ca sem bulb = do
  let rq = myQuery sem bulb
  if isCompleteColor ca
    then setColor' $ customColor ca -- FIXME: handle named colors
    else rq "getLight" (getLight bulb) $ \sl -> do
    let orig = justColor $ color16toFrac $ slColor sl
        newC = orig `combineColors` customColor ca
    setColor' newC
  where
    ra = myAction sem bulb
    dur = 1000
    setColor' newC = ra "setColor" (setColor bulb (colorFracTo16 $ definitelyColor newC) dur) (atomically $ signalTSem sem)

cmdWave :: Waveform -> C.PulseArg -> TSem -> Bulb -> IO ()
cmdWave = undefined

cmd2func :: C.LiteCmd -> TSem -> Bulb -> IO ()
cmd2func C.CmdList = cmdList
cmd2func C.CmdOn = cmdPower True
cmd2func C.CmdOff = cmdPower False
cmd2func (C.CmdColor ca) = cmdColor ca
cmd2func (C.CmdPulse pa) = cmdWave Pulse pa
cmd2func (C.CmdBreathe pa) = cmdWave Sine pa

lsHeader :: IO ()
lsHeader = do
  let fmtStrn = fmtStr ++ "\n"
      dashes = replicate 80 '-'
  tr $ printf fmtStrn "Label" "Pwr" "Color" "Temp" "Uptime" "DevID" "FW" "HW"
  tr $ printf fmtStrn dashes dashes dashes dashes dashes dashes dashes dashes

hdrIfNeeded :: C.LiteCmd -> IO ()
hdrIfNeeded C.CmdList = lsHeader
hdrIfNeeded _ = return ()

main = do
  args <- C.parseCmdLine
  let ifname = fromMaybe (T.pack "en1") $ C.aInterface args
      cmd = C.aCmd args
      func = cmd2func cmd
  lan <- openLan ifname
  hdrIfNeeded cmd
  -- discoverBulbs lan myCb
  -- lsBulbs lan
  -- forever $ threadDelay 1000000000

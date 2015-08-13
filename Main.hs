{-# LANGUAGE StandaloneDeriving #-}

import Control.Concurrent
import Control.Monad ( when, forever )
import Data.Hourglass
{-
    ( ElapsedP(ElapsedP),
      ISO8601_DateAndTime(ISO8601_DateAndTime),
      timePrint )
-}
import Data.Word ( Word16, Word64 )
import Text.Printf ( printf )

import Lifx.Lan.LowLevel

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

nsToDuration :: NanoSeconds -> (Int, Duration)
nsToDuration (NanoSeconds ns) =
  (fromIntegral days, Duration (Hours hours) (Minutes minutes)
                      (Seconds seconds) (NanoSeconds nanos))
  where (s, nanos) = ns `quotRem` 1000000000
        (m, seconds) = s `quotRem` 60
        (h, minutes) = m `quotRem` 60
        (days, hours) = h `quotRem` 24

prLight :: StateLight -> (String, String, String) -- label, power, color
prLight sl = (label, power, color)
  where label = show $ slLabel sl
        power = if slPower sl == 0 then "Off" else "On"
        color = printf "%5.1f %5.1f %5.1f %4dK" h s b k
        hsbk = slColor sl
        h = toDbl (hue hsbk) / m * 360
        s = toDbl (saturation hsbk) / m * 100
        b = toDbl (brightness hsbk) / m * 100
        k = kelvin hsbk
        m = toDbl (maxBound :: Word16)
        toDbl x = (fromIntegral x) :: Double

prHostInfo :: StateHostInfo -> String -- temp
prHostInfo shi = printf "%4.1f°C" (temp :: Double)
  where temp = (fromIntegral $ shiMcuTemperature shi) / 100

prInfo :: StateInfo -> String -- uptime
prInfo si = fmtDur $ nsToDuration $ fromIntegral $ siUptime si
  where fmtDur (days, Duration (Hours hours) (Minutes minutes)
                      (Seconds seconds) _) =
          printf "%dd%dh%dm%ds" days hours minutes seconds

prHostFirmware :: StateHostFirmware -> String -- firmware version
prHostFirmware shf = printf "%6x" (shfVersion shf)

prVersion :: StateVersion -> String -- hardware version
prVersion sv = printf "%d.%d.%d" (svVendor sv) (svProduct sv) (svVersion sv)

fmtStr = "%-18.18s %-3.3s %-21.21s %-6.6s %-10.10s %-4.4s %-6.6s %-5.5s"

prBulb :: StateHostInfo
          -> StateLight
          -> StateHostFirmware
          -> StateVersion
          -> StateInfo
          -> String
prBulb shi sl shf sv si =
  printf fmtStr label power color temp uptime devid fw vers
  where (label, power, color) = prLight sl
        temp = prHostInfo shi
        uptime = prInfo si
        devid = "todo"
        fw = prHostFirmware shf
        vers = prVersion sv

lsCb :: Bulb -> IO ()
lsCb bulb =
  getHostInfo bulb $ \shi ->
    getLight bulb $ \sl ->
      getHostFirmware bulb $ \shf ->
        getVersion bulb $ \sv ->
          getInfo bulb $ \si ->
            putStrLn $ prBulb shi sl shf sv si

lsBulbs :: Lan -> IO ()
lsBulbs lan = do
  printf fmtStr "Label" "Pwr" "Color" "Temp" "Uptime" "DevID" "Firmware" "Version"
  putStrLn ""
  discoverBulbs lan lsCb

main = do
  lan <- openLan' "en1" Nothing (Just putStrLn)
  -- discoverBulbs lan myCb
  lsBulbs lan
  forever $ threadDelay 1000000000

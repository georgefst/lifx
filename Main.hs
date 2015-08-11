{-# LANGUAGE StandaloneDeriving #-}

import Control.Concurrent
import Control.Monad ( when, forever )
import Data.Hourglass
{-
    ( ElapsedP(ElapsedP),
      ISO8601_DateAndTime(ISO8601_DateAndTime),
      timePrint )
-}
import Data.Word ( Word64 )
import Text.Printf ( printf )

import Lifx.Lan.LowLevel

myTime :: Word64 -> String
myTime nanos =
  let (q, r) = quotRem nanos 1000000000
      elapp = ElapsedP (fromIntegral q) (fromIntegral r)
  in timePrint ISO8601_DateAndTime elapp


myCb :: Bulb -> IO ()
myCb bulb = do
  putStrLn (show bulb)
  getHostInfo bulb $ \shi -> do
    putStrLn (show shi)
    getLight bulb $ \sl -> do
      putStrLn (show sl)
      setPower bulb True $ do
        setColor bulb (HSBK 32768 65535 65535 3000) 0 $ do
          -- positive numbers mean more time spent on original color
          let swf = SetWaveform False (HSBK 0 0 65535 9000) 1000 10 (-20000) Pulse
          setWaveform bulb swf $ do
            getHostFirmware bulb $ \shf -> do
              putStrLn (show shf)
              let vHex = printf "%x" (shfVersion shf)
              putStrLn $ "build " ++ (myTime $ shfBuild shf) ++ ", version " ++ vHex
              getWifiFirmware bulb $ \swf -> do
                putStrLn (show swf)
                let vHex' = printf "%x" (swfVersion swf)
                putStrLn $ "build " ++ (myTime $ swfBuild swf) ++ ", version " ++ vHex'
                getVersion bulb $ \sv -> do
                  putStrLn (show sv)
                  printf "%x %x %x\n" (svVendor sv) (svProduct sv) (svVersion sv)
                  getInfo bulb $ \si -> do
                    putStrLn (show si)
                    putStrLn $ "current time = " ++ (myTime $ siTime si)
                    print $ nsToDuration $ fromIntegral $ siUptime si
                    print $ nsToDuration $ fromIntegral $ siDowntime si
                    putStrLn "done!"

deriving instance Show Duration

nsToDuration :: NanoSeconds -> Duration
nsToDuration (NanoSeconds ns) =
  Duration (Hours hours) (Minutes minutes) (Seconds seconds) (NanoSeconds nanos)
  where (s, nanos) = ns `quotRem` 1000000000
        (m, seconds) = s `quotRem` 60
        (hours, minutes) = m `quotRem` 60

main = do
  lan <- openLan
  discoverBulbs lan myCb
  forever $ threadDelay maxBound

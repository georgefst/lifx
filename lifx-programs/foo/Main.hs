{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Data.Char
import qualified Data.Text as T

import Lifx.Lan
import Lifx.Lan.LowLevel
import Lifx
import Lifx.Cloud

st = StateTransition
     { sPower = Nothing
     , sColor = blue
     , sDuration = 1.0
     }

fromRight = either error id

main = do
  {-
  lc <- openLanConnection defaultLanSettings
  threadDelay 1000000
  -}
  lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
  let lifxToken = fromRight $ fromText $ T.pack $ takeWhile (not . isSpace) lifxTokenStr
      cs = defaultCloudSettings { csToken = lifxToken }
  lc <- openCloudConnection cs
  li <- listLights lc [SelAll] needEverything
  print li
  putStrLn ""
  tr <- setStates lc [([SelAll], st)]
  {-
  tr <- togglePower lc [SelAll] 3.0
  tr <- effect lc [SelAll] defaultEffect { eColor = red
                                         , eType = Breathe
                                         , eCycles = 5
                                         , eFromColor = green
                                         , ePowerOn = False
                                         }
  -}
  print tr

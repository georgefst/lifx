{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent

import Lifx.Lan
import Lifx.Lan.LowLevel
import Lifx

ls = LanSettings
     { lsIfName      = "en1"
     , lsLog         = putStrLn
     , lsPort        = 56700
     , lsListScenes  = undefined
     , lsRetryParams = defaultRetryParams
     }

st = StateTransition
     { sPower = Nothing
     , sColor = blue
     , sDuration = 1.0
     }

main = do
  lc <- openLanConnection ls
  threadDelay 1000000
  li <- listLights lc [SelAll] needEverything
  print li
  tr <- setStates lc [([SelAll], st)]
  print tr

{-# LANGUAGE OverloadedStrings #-}

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

main = do
  lc <- openLanConnection ls
  li <- listLights lc [SelAll] needEverything
  print li

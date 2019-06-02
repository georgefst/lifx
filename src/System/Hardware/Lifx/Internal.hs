{-# OPTIONS_HADDOCK hide #-}

{-|
Module      : Lifx
Description : Internal functions for use by LIFX-related packages.
Copyright   : © Patrick Pelletier, 2016
License     : BSD3
Maintainer  : code@funwithsoftware.org
Stability   : experimental
Portability : GHC

This module contains types and functions for use by other
LIFX-related packages, such as @lifx-lan@ and @lifx-cloud@.
No guarantees are made about the stability of the interfaces,
and it should not be used by external packages.
-}

module System.Hardware.Lifx.Internal
       ( padByteString
       , fmt
       , parseAllMaybe
       , Config (..)
       , configFile
       , getConfig
       , minKelvin, maxKelvin
       ) where

import System.Hardware.Lifx.Lan.LowLevel.Internal
import System.Hardware.Lifx.Util
import System.Hardware.Lifx.Config
import System.Hardware.Lifx.Types

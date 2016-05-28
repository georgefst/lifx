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

module Lifx.Internal
       ( padByteString
       , fmt
       , selParse
       , parseAllMaybe
       , Config (..)
       , configFile
       , getConfig
       , minKelvin, maxKelvin
       ) where

import Lifx.Util
import Lifx.SelectorParser
import Lifx.Config
import Lifx.Types

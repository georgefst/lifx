{-|
Module      : Lifx
Description : Types for use by packages that interface with LIFX smart bulbs
Copyright   : © Patrick Pelletier, 2016
License     : BSD3
Maintainer  : code@funwithsoftware.org
Stability   : experimental
Portability : GHC

This module contains some types for representing basic concepts used
in interfacing with <https://www.lifx.com/ LIFX smart light bulbs>.
-}

module System.Hardware.Lifx
       ( -- * Basic types
         LifxException(..)
       , ParamError (..)
       , Power (..)
       , ColorChannel
       , FracSeconds
       , Interface
         -- * Colors
       , HSBK (..)
       , Color , PartialColor
       , combineColors, emptyColor , isEmptyColor, isCompleteColor
         -- ** Named colors
         -- | The same <https://community.lifx.com/t/comprehensive-list-of-recognized-color-names/1067/2 nine named colors>
         -- recognized by the LIFX Cloud API.  They only specify the hue
         -- and saturation.  (Or in the case of white, just the saturation.)
       , white, red, orange, yellow, green, cyan, blue, purple, pink
         -- * IDs
       , LifxId (..)
       , DeviceId, GroupId, LocationId, Label, AccessToken
       , SceneId (..)
         -- * Products
       , Capabilities (..)
       ) where

import System.Hardware.Lifx.Lan.LowLevel
import System.Hardware.Lifx.Types

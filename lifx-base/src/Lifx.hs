{-|
Module      : Lifx
Description : Types for use by packages that interface with LIFX smart bulbs
Copyright   : (c) Patrick Pelletier, 2016
License     : BSD3
Maintainer  : code@funwithsoftware.org
Stability   : experimental
Portability : GHC

This module contains some types for representing basic concepts used
in interfacing with <https://www.lifx.com/ LIFX smart light bulbs>.
It also contains the 'Connection' typeclass, which represents a
connection to a collection of LIFX bulbs.  For an implementation of
the 'Connection' class, you'll need another package, such as
@lifx-lan@ or @lifx-cloud@.
-}

module Lifx
       ( -- * Basic types
         LifxException(..)
       , Power (..)
       , LiFrac
       , FracSeconds
         -- * Colors
       , HSBK (..)
       , Color , MaybeColor
       , combineColors, emptyColor , isEmptyColor, isCompleteColor
       , colorToText
       , parseColor
         -- ** Named colors
         -- | The same <https://community.lifx.com/t/comprehensive-list-of-recognized-color-names/1067/2 nine named colors>
         -- recognized by the LIFX Cloud API.  They only specify the hue
         -- and saturation.  (Or in the case of white, just the saturation.)
       , white, red, orange, yellow, green, cyan, blue, purple, pink
         -- * IDs
       , LifxId (..)
       , DeviceId, GroupId, LocationId, Label, AuthToken
       , SceneId (..)
         -- * Selectors
       , Selector (..)
       , selectorToText
       , selectorsToText
       , parseSelector
       , parseSelectors
         -- * Products
       , Product (..)
       , Capabilities (..)
       , productFromId
         -- * Effects
       , EffectType (..)
       , Effect (..)
       , defaultEffect
         -- * Connections
       , Connection (..)
       , InfoNeeded (..)
       , needEverything
       , LightInfo (..)
       , StateTransition (..)
       , Result (..)
       , Status (..)
       , StateTransitionResult (..)
       , Scene (..)
       , SceneState (..)
       ) where

import Lifx.ColorParser
import Lifx.SelectorParser
import Lifx.Util
import Lifx.Types
import Lifx.ProductTable

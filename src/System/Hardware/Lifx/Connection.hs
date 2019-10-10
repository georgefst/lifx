{-|
Module      : System.Hardware.Lifx.Connection
Description : Typeclass for a connection to LIFX bulbs
Copyright   : © Patrick Pelletier, 2016
License     : BSD3
Maintainer  : code@funwithsoftware.org
Stability   : experimental
Portability : GHC

This module contains the 'Connection' typeclass, which represents a
connection to a collection of LIFX bulbs.  For an implementation of
the 'Connection' class, you'll need another package, such as
@lifx-lan@ or @lifx-cloud@.
-}

{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.Lifx.Connection (
                         InfoNeeded (..)
                       , needEverything
                       , LightInfo (..)
                       , StateTransition (..)
                       , Result (..)
                       , Status (..)
                       , Scene (..)
                       , EffectType (..)
                       , Effect (..)
                       , defaultEffect
                       , Direction (..)
                       ) where

import Data.Hourglass
import qualified Data.Text as T
import qualified Data.UUID.Types as U
import Data.Version

import System.Hardware.Lifx.Lan.LowLevel hiding (Pulse)
import System.Hardware.Lifx.Types

-- | The direction that the 'cycleLights' method will go in.
data Direction = Forward | Backward
               deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | Hints about what information is needed from 'listLights'.
-- A backend can use these hints to avoid collecting unneeded
-- information when generating 'LightInfo'.  However, beware that this is
-- only a hint, and the backend is free to return more information
-- than requested.  Also, the backend might still return
-- 'Nothing' for fields which are requested, in case of error, or
-- in case the backend does not support that field.
data InfoNeeded = NeedLabel | NeedPower | NeedColor | NeedGroup | NeedLocation
                | NeedProduct | NeedTemperature | NeedUptime
                | NeedFirmwareVersion | NeedHardwareVersion
                deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | A list of all possible values of 'InfoNeeded', thus requesting as much
-- information as possible be returned.
needEverything :: [InfoNeeded]
needEverything = [minBound .. maxBound]

-- | Information about a light, returned by 'listLights'.
-- Most fields are 'Maybe', and may be omitted if there is an error,
-- if the field is not supported by the backend, or if the field
-- was not requested with an 'InfoNeeded'.
data LightInfo =
  LightInfo
  { lId :: DeviceId       -- ^ MAC address of bulb.  Primary way of
                          -- identifying and addressing bulbs.
  , lUuid :: Maybe U.UUID -- ^ An alternate way of identifying a bulb. Doesn't
                          -- seem all that useful, but the Cloud API provides it.
  , lLabel :: Maybe Label -- ^ Human-readable label given to the bulb.
  , lConnected :: Bool
  , lPower :: Maybe Power
  , lColor :: PartialColor
  , lGroupId :: Maybe GroupId
  , lGroup :: Maybe Label
  , lLocationId :: Maybe LocationId
  , lLocation :: Maybe Label
  , lLastSeen :: DateTime
  , lSecondsSinceSeen :: FracSeconds
  , lProduct :: Maybe Product     -- ^ information about the model of bulb
  , lTemperature :: Maybe Double  -- ^ in degrees Celsius
  , lUptime :: Maybe FracSeconds  -- ^ time since power was applied to bulb
  , lFirmwareVersion :: Maybe Version
  , lHardwareVersion :: Maybe Int
  } deriving (Eq, Ord, Show, Read)


-- | A change in color and/or power, over a period of time.
data StateTransition =
  StateTransition
  { sPower :: Maybe Power
  , sColor :: PartialColor
  , sDuration :: FracSeconds
  } deriving (Eq, Ord, Show, Read)

-- | Result of executing an operation.
data Result =
  Result
  { rId :: DeviceId       -- ^ ID of affected light
  , rLabel :: Maybe Label -- ^ Label of affected light
  , rStatus :: Status     -- ^ Whether the operation was successful on this light
  } deriving (Eq, Ord, Show, Read)

-- | Whether an operation was successful on a particular light.
data Status = Ok | TimedOut | Offline
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | The shape of the waveform of an 'Effect'.
data EffectType = Pulse   -- ^ a square wave
                | Breathe -- ^ a sine wave
                  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | Specifies details of the effect performed by 'effect'.
data Effect =
  Effect
  { eType :: EffectType      -- ^ The shape of the waveform.  Default 'Pulse'.
  , eColor :: PartialColor     -- ^ The color of the effect.
  , eFromColor :: PartialColor -- ^ The color to start from.  'emptyColor'
                             -- means start from the current color.
                             -- Default 'emptyColor'.
  , ePeriod :: FracSeconds   -- ^ The period of the waveform in seconds.
                             -- Default 1.0.
  , eCycles :: Double        -- ^ The total duration of the effect, as
                             -- multiples of the period.  Default 1.0.
  , ePersist :: Bool         -- ^ 'False' means return to original color
                             -- when effect is complete.  Default 'False'.
  , ePowerOn :: Bool         -- ^ Turn power on if it is off?  Default 'True'.
  , ePeak :: Double          -- ^ For 'Breathe', specifies where in the period
                             -- the effect is brightest.  For 'Pulse', specifies
                             -- the duty cycle of the pulse on @LanConnection@, or
                             -- is ignored on @CloudConnection@.  0.0 - 1.0.
                             -- Default 0.5.
  } deriving (Eq, Ord, Show, Read)

-- | Returns an 'Effect' with default settings.
defaultEffect :: Effect
defaultEffect = Effect
  { eType = Pulse
  , eColor = emptyColor
  , eFromColor = emptyColor
  , ePeriod = 1.0
  , eCycles = 1.0
  , ePersist = False
  , ePowerOn = True
  , ePeak = 0.5
  }

-- | A predefined set of state changes for specific lights.
data Scene =
  Scene
  { scId :: SceneId           -- ^ A unique identifier for this scene.
  , scName :: T.Text          -- ^ The name of this scene.
  , scUpdatedAt :: DateTime   -- ^ Most recent time the scene was updated.
  , scCreatedAt :: DateTime   -- ^ Time the scene was originally created.
  , scAccount :: Maybe U.UUID -- ^ A unique identifier for the user who
                              -- created this scene.  May be 'Nothing' if
                              -- this is not a cloud scene.
  } deriving (Eq, Ord, Show, Read)

{-|
Module      : Lifx.Connection
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

module Lifx.Connection ( Connection (..)
                       , InfoNeeded (..)
                       , needEverything
                       , LightInfo (..)
                       , StateTransition (..)
                       , Result (..)
                       , Status (..)
                       , StateTransitionResult (..)
                       , Scene (..)
                       , SceneState (..)
                       , EffectType (..)
                       , Effect (..)
                       , defaultEffect
                       , Direction (..)
                       ) where

import Control.Exception
import Data.Fixed
import Data.Hourglass
import Data.List
import qualified Data.Text as T
import qualified Data.UUID.Types as U
import Data.Version

import Lifx.Types
import Lifx.Util

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


data StateTransition =
  StateTransition
  { sPower :: Maybe Power
  , sColor :: PartialColor
  , sDuration :: FracSeconds
  } deriving (Eq, Ord, Show, Read)

data Result =
  Result
  { rId :: DeviceId
  , rLabel :: Maybe Label
  , rStatus :: Status
  } deriving (Eq, Ord, Show, Read)

data Status = Ok | TimedOut | Offline
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

data StateTransitionResult =
  StateTransitionResult
  { tOperation :: ([Selector], StateTransition)
  , tResults :: [Result]
  } deriving (Eq, Ord, Show, Read)

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

data Scene =
  Scene
  { scId :: SceneId
  , scName :: T.Text
  , scUpdatedAt :: DateTime
  , scCreatedAt :: DateTime
  , scAccount :: Maybe U.UUID
  , scStates :: [SceneState]
  } deriving (Eq, Ord, Show, Read)

data SceneState =
  SceneState
  { ssSel   :: Selector
  , ssPower :: Maybe Power
  , ssColor :: PartialColor
  } deriving (Eq, Ord, Show, Read)

----------------------------------------------------------------------

-- | Class representing a connection to a collection of bulbs.
-- In the case of a LAN connection, this would be all bulbs on the LAN.
-- In the case of a cloud connection, this would be all bulbs associated
-- with the cloud account for a particular access token.
class Connection t where
  -- | Retrieve information about some or all lights.  Corresponds to
  -- <http://api.developer.lifx.com/docs/list-lights List Lights> endpoint.
  -- Beware that on a @LanConnection@, it takes time for lights to be
  -- discovered, so the list of lights will be empty immediately after
  -- the connection is created.
  listLights :: t                  -- ^ The connection.
                -> [Selector]      -- ^ The lights to list.
                -> [InfoNeeded]    -- ^ A hint about what information is desired
                                   -- in the results.  This hint is used
                                   -- by @LanConnection@, but is ignored by
                                   -- @CloudConnection@.
                -> IO [LightInfo]

  -- | Apply a state transition to a set of lights.  Corresponds to
  -- <http://api.developer.lifx.com/docs/set-state Set State> endpoint.
  setState :: t                    -- ^ The connection.
              -> [Selector]        -- ^ The lights to operate on.
              -> StateTransition   -- ^ The state to apply to the lights.
              -> IO [Result]
  setState conn sels trans = do
    [tr] <- setStates conn [(sels, trans)]
    return (tResults tr)

  -- | Apply one or more state transitions to different sets of lights
  -- simultaneously.  Corresponds to
  -- <http://api.developer.lifx.com/docs/set-states Set States> endpoint.
  setStates :: t                                  -- ^ The connection.
               -> [([Selector], StateTransition)] -- ^ Pairs of state
                                                  -- transitions and the lights
                                                  -- to apply them to.
               -> IO [StateTransitionResult]

  -- | Turn the specified lights on if all of them are off.  Turn the
  -- specified lights off if any of them are on.  Corresponds to
  -- <http://api.developer.lifx.com/docs/toggle-power Toggle Power> endpoint.
  togglePower :: t              -- ^ The connection.
                 -> [Selector]  -- ^ The lights to operate on.
                 -> FracSeconds -- ^ Duration of fade.
                 -> IO [Result]
  togglePower conn sels dur = do
    -- https://community.lifx.com/t/toggle-power-endpoint-when-existing-state-is-mixed/1097
    li <- listLights conn sels [NeedPower]
    let available (LightInfo { lConnected = True, lPower = (Just _ ) }) = True
        available _ = False
        (up, down) = partition available li
        anyLitesOn = Just On `elem` map lPower up
        transition =
          StateTransition { sPower = Just (if anyLitesOn then Off else On)
                          , sColor = emptyColor
                          , sDuration = dur
                          }
        timedOut x = Result (lId x) Nothing TimedOut
    results <- setState conn (map (SelDevId . lId) up) transition
    return $ map timedOut down ++ results

  -- | Perform the specified effect on the specified lights.  Corresponds to
  -- <http://api.developer.lifx.com/docs/breathe-effect Breathe Effect> or
  -- <http://api.developer.lifx.com/docs/pulse-effect Pulse Effect> endpoint,
  -- depending on 'EffectType'.
  effect :: t              -- ^ The connection.
            -> [Selector]  -- ^ The lights to operate on.
            -> Effect      -- ^ The effect to perform.
            -> IO [Result]

  -- | Lists the scenes associated with this 'Connection'.  Corresponds to
  -- <http://api.developer.lifx.com/docs/list-scenes List Scenes> endpoint.
  listScenes :: t -- ^ The connection.
                -> IO [Scene]

  -- | Activates a specified scene.  Corresponds to
  -- <http://api.developer.lifx.com/docs/activate-scene Activate Scene>
  -- endpoint.
  activateScene :: t              -- ^ The connection.
                   -> SceneId     -- ^ ID of the scene to activate.
                   -> FracSeconds -- ^ Duration of fade.
                   -> IO [Result]
  activateScene conn sid dur = do
    scenes <- listScenes conn
    let sceneIds = map scId scenes
        mscene = sid `lookup` zip sceneIds scenes
    scene <- case mscene of
              Nothing -> throwIO $ SelectorNotFound $ SelSceneId sid
              Just x -> return x
    let states = map (sceneStateToStatePair dur) (scStates scene)
    trs <- setStates conn states
    return $ concatMap tResults trs

  -- | Determines which 'StateTransition' most closely matches the
  -- current state of the specified lights, and then activates the
  -- next (or previous) 'StateTransition' in the list, wrapping around
  -- to the beginning (or end) if necessary.  Corresponds to
  -- <http://api.developer.lifx.com/docs/cycle Cycle> endpoint.
  cycleLights :: t                    -- ^ The connection.
                 -> [Selector]        -- ^ The lights to operate on.
                 -> [StateTransition] -- ^ States to cycle through
                 -> Direction         -- ^ Go forward or backward?
                 -> IO [Result]
  cycleLights conn sels states dir = do
    -- TODO: refactor
    li <- listLights conn sels [NeedPower, NeedColor]
    let available (LightInfo { lConnected = True, lPower = (Just _ ) }) = True
        available _ = False
        (up, down) = partition available li
        transition = nextState dir states up
        timedOut x = Result (lId x) Nothing TimedOut
    results <- setState conn (map (SelDevId . lId) up) transition
    return $ map timedOut down ++ results

  -- | Change the label of the specified light.  Throws 'Unsupported'
  -- in the cloud backend.
  setLabel :: t                    -- ^ The connection.
              -> DeviceId          -- ^ The light to operate on.
              -> Label             -- ^ The new label for the light.
              -> IO Result
  setLabel _ _ _ = throwIO $ Unsupported "setLabel"

  -- | Terminates the 'Connection' and frees any resources associated
  -- with it.
  closeConnection :: t        -- ^ The connection to terminate.
                     -> IO ()

sceneStateToStatePair :: FracSeconds
                         -> SceneState
                         -> ([Selector], StateTransition)
sceneStateToStatePair dur scenest =
  ([ssSel scenest],
   StateTransition { sPower = ssPower scenest
                   , sColor = ssColor scenest
                   , sDuration = dur
                   })

comparePower :: Maybe Power -> Maybe Power -> ColorChannel
comparePower (Just On) (Just Off) = 1
comparePower (Just Off) (Just On) = 1
comparePower _ _ = 0

compareColor :: PartialColor -> PartialColor -> ColorChannel
compareColor x y =
  compareComponent360 (hue x) (hue y) +
  compareComponent (saturation x) (saturation y) 1 +
  compareComponent (brightness x) (brightness y) 1 +
  compareComponent (kelvin x) (kelvin y) (9000 - 2500)

compareComponent :: Maybe ColorChannel -> Maybe ColorChannel -> ColorChannel -> ColorChannel
compareComponent (Just x) (Just y) scale = diff * diff
  where diff = (x - y) / scale
compareComponent _ _ _ = 0

compareComponent360 :: Maybe ColorChannel -> Maybe ColorChannel -> ColorChannel
compareComponent360 (Just x) (Just y) = min (diff1 * diff1) (diff2 * diff2)
  where diff1 = (x - y) / 360
        diff2 = ((x + 180) `mod'` 360 - (y + 180) `mod'` 360) / 360
compareComponent360 _ _ = 0

compareStates :: StateTransition -> LightInfo -> ColorChannel
compareStates st li =
  comparePower (sPower st) (lPower li) +
  compareColor (sColor st) (lColor li)

evaluateState :: StateTransition -> [LightInfo] -> ColorChannel
evaluateState st lis = sum $ map (compareStates st) lis

bestState :: [StateTransition] -> [LightInfo] -> Int
bestState sts lis = snd $ minimum $ zipWith with sts [0..]
  where with st idx = (evaluateState st lis, idx)

nextState :: Direction -> [StateTransition] -> [LightInfo] -> StateTransition
nextState Forward  sts lis = sts !! ((bestState sts lis + 1) `mod` length sts)
nextState Backward sts lis = sts !! ((bestState sts lis - 1) `mod` length sts)

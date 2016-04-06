{-# LANGUAGE OverloadedStrings #-}

module Lifx.Connection (Connection (..)) where

import Control.Exception
import Data.Fixed
import Data.List

import Lifx.Types
import Lifx.Util

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

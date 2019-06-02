{-|
Module      : System.Hardware.Lifx.Lan.Lowlevel
Description : Low-level access to the LIFX Lan Protocol
Copyright   : © Patrick Pelletier, 2016
License     : BSD3
Maintainer  : code@funwithsoftware.org
Stability   : experimental
Portability : GHC

This module provides a low-level way of using the LIFX Lan Protocol.
-}

module System.Hardware.Lifx.Lan.LowLevel
    ( -- * Lans and Bulbs
      openLan,
      openLan',
      closeLan,
      discoverBulbs,
      deviceId,
      Lan,
      Bulb,
      -- * Queries and actions
      -- | These functions correspond directly to
      -- <https://lan.developer.lifx.com/ Lan Protocol> messages which
      -- can be sent to the bulb.  Because the Lan Protocol uses
      -- <https://en.wikipedia.org/wiki/User_Datagram_Protocol UDP>, these
      -- functions are unreliable.  Either your callback will get called at
      -- some point in the future, or it will never be called.  You have to
      -- decide when to time out and when to retry.  The wrappers
      -- 'reliableAction' and 'reliableQuery' can help with this.

      -- ** Queries
      -- | These functions do not change the state of the bulb.
      -- The callback is given some information returned from the bulb.
      getHostInfo,
      getHostFirmware,
      getWifiFirmware,
      getVersion,
      getInfo,
      getLight,
      getGroup,
      getLocation,
      echoRequest,
      -- ** Actions
      -- | These functions change the state of the bulb.  The callback
      -- is given no data, but indicates that the bulb has acknowledged
      -- receipt of the action.
      setPowerGG,
      setLabel,
      setColorGG,
      setWaveform,
      -- * Types
      Power(..),
      Interface,
      HSBK(..),
      HSBK16,
      DeviceId,
      GroupId,
      LocationId,
      Label,
      LifxId(..),
      Waveform(..),
      SetWaveform(..),
      StateInfo(..),
      StateVersion(..),
      StateWifiFirmware(..),
      StateHostFirmware(..),
      StateLight(..),
      StateHostInfo(..),
      StateGroup(..),
      StateLocation(..),
      -- * Reliability wrappers
      reliableAction,
      reliableQuery,
      RetryParams(..),
      defaultRetryParams
      ) where

import System.Hardware.Lifx.Lan.LowLevel.BaseTypes
import System.Hardware.Lifx.Lan.LowLevel.Types
import System.Hardware.Lifx.Lan.LowLevel.Messages
import System.Hardware.Lifx.Lan.LowLevel.Protocol

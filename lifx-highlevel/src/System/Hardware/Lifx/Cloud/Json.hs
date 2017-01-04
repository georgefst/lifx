{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.Lifx.Cloud.Json where

import Data.Aeson hiding (Result)
import Data.Aeson.Types (Parser, Pair)
import Data.Hourglass
import Data.Maybe
import qualified Data.Text as T
import qualified Data.UUID.Types as U
import Text.Read hiding (String)

import System.Hardware.Lifx
import System.Hardware.Lifx.Cloud.Util
import System.Hardware.Lifx.Connection

instance FromJSON Power where
  parseJSON (String "on") = return On
  parseJSON (String "off") = return Off
  parseJSON (String txt) =
    fail $ "expected power to be 'on' or 'off', but got " ++ show txt
  parseJSON _ = fail "expected a JSON string for power"

instance ToJSON Power where
  toJSON On  = String "on"
  toJSON Off = String "off"

instance FromJSON Direction where
  parseJSON (String "forward") = return Forward
  parseJSON (String "backward") = return Backward
  parseJSON (String txt) =
    fail $ "expected direction to be 'forward' or 'backward', but got " ++ show txt
  parseJSON _ = fail "expected a JSON string for direction"

instance ToJSON Direction where
  toJSON Forward = String "forward"
  toJSON Backward = String "backward"

instance FromJSON Selector where
  parseJSON (String txt) =
    case parseSelector txt of
     Nothing -> fail "couldn't parse selector"
     Just sel -> return sel

newtype WrapSelectors = WrapSelectors { unWrapSelectors :: [Selector] }

instance FromJSON WrapSelectors where
  parseJSON (String txt) =
    case parseSelectors txt of
     Nothing -> fail "couldn't parse selectors"
     Just sels -> return (WrapSelectors sels)

parseIdStruct :: FromJSON a => Maybe Value -> Parser (Maybe a, Maybe Label)
parseIdStruct (Just (Object v)) = do
  i <- v .:? "id"
  n <- v .:? "name"
  return (i, n)
parseIdStruct _ = return (Nothing, Nothing)

combineColorBrightness :: Maybe Value -> Maybe Double -> Parser PartialColor
combineColorBrightness c b = do
  c' <- parseC c
  return $ addBrightness c' b
  where addBrightness cc Nothing = cc
        addBrightness cc br@(Just _ ) = cc { brightness = br }
        parseC Nothing = return emptyColor
        parseC (Just (Object v)) = do
          myHue        <- v .:? "hue"
          mySaturation <- v .:? "saturation"
          myBrightness <- v .:? "brightness"
          myKelvin     <- v .:? "kelvin"
          return $ HSBK myHue mySaturation myBrightness myKelvin
        parseC _ = fail "expected a JSON object for color"

parseColorBrightness :: Object -> Parser PartialColor
parseColorBrightness v = do
    myColorObj         <- v .:? "color"
    myBrightness       <- v .:? "brightness"
    combineColorBrightness myColorObj myBrightness

parseCaps :: Value -> Parser Capabilities
parseCaps (Object v) = do
  hasColor  <- v .: "has_color"
  hasVCTemp <- v .: "has_variable_color_temp"
  return $ Capabilities { cHasColor = hasColor
                        , cHasVariableColorTemp = hasVCTemp }
parseCaps _ = fail "expected a JSON object for capabilities"

instance FromJSON LightInfo where
  parseJSON (Object v) = do
    myId               <- v .:  "id"
    myUuid             <- parseUuid (Just v)
    myLabel            <- v .:? "label"
    myConnected        <- v .:  "connected"
    myPower            <- v .:? "power"
    myGroupStruct      <- v .:? "group"
    myLocationStruct   <- v .:? "location"
    myLastSeenStr      <- v .:  "last_seen"
    mySecondsSinceSeen <- v .:  "seconds_since_seen"
    myColor            <- parseColorBrightness v
    p                  <- v .: "product"
    myProductName      <- p .: "name"
    myProductCompany   <- p .: "company"
    myProductIdent     <- p .: "identifier"
    myCapabilities     <- p .: "capabilities"
    myFirmwareVersStr  <- v .:? "firmware_version"
    myHardwareVersion  <- v .:? "hardware_version"

    myTemperature      <- v .:? "temperature"
    myUptime           <- v .:? "uptime"

    (myGroupId, myGroup)       <- parseIdStruct myGroupStruct
    (myLocationId, myLocation) <- parseIdStruct myLocationStruct

    myLastSeen <- case timeParseE MyISO8601_DateAndTime myLastSeenStr of
                   Right (x, _ ) -> return x
                   Left (tfe , msg) -> fail
                                     $ msg ++ " when parsing " ++ show tfe
                                     ++ " in '" ++ myLastSeenStr ++ "'"

    myFirmwareVersion <- case myFirmwareVersStr of
                          Nothing -> return Nothing
                          Just s -> case readEither s of
                                     Left msg -> fail msg
                                     Right vers -> return $ Just vers

    myCaps <- parseCaps myCapabilities

    let myProduct =
          Product
          { pCompanyName  = myProductCompany
          , pProductName  = myProductName
          , pIdentifier   = myProductIdent
          , pCapabilities = myCaps
          }

    return $ LightInfo
           { lId               = myId
           , lUuid             = myUuid
           , lLabel            = myLabel
           , lConnected        = myConnected
           , lPower            = myPower
           , lColor            = myColor
           , lGroupId          = myGroupId
           , lGroup            = myGroup
           , lLocationId       = myLocationId
           , lLocation         = myLocation
           , lLastSeen         = myLastSeen
           , lSecondsSinceSeen = mySecondsSinceSeen
           , lProduct          = Just myProduct
           , lTemperature      = myTemperature
           , lUptime           = myUptime
           , lFirmwareVersion  = myFirmwareVersion
           , lHardwareVersion  = myHardwareVersion
           }

  parseJSON _ = fail "expected a JSON object for light"

instance FromJSON StateTransition where
  parseJSON (Object v) = do
    myPower      <- v .:? "power"
    myColorTxt   <- v .:? "color"
    myBrightness <- v .:? "brightness"
    myDuration   <- v .:? "duration" .!= 1.0

    myColor1 <- case myColorTxt of
                 Nothing -> return emptyColor
                 Just txt -> case parseColor txt of
                              Nothing -> fail "couldn't parse color"
                              Just c -> return c

    let myColor2 = emptyColor { brightness = myBrightness }
        myColor = myColor1 `combineColors` myColor2

    return $ StateTransition myPower myColor myDuration

  parseJSON _ = fail "expected a JSON object for state transition"

(.=?) :: ToJSON a => T.Text -> Maybe a -> Maybe Pair
name .=? Nothing = Nothing
name .=? (Just x) = Just (name .= x)

(.=!) :: ToJSON a => T.Text -> a -> Maybe Pair
name .=! x = Just (name .= x)

objectMaybe :: [Maybe Pair] -> Value
objectMaybe = object . catMaybes

stateTransitionToPairs :: StateTransition -> [Pair]
stateTransitionToPairs st =
  catMaybes
  [ "power" .=? sPower st
  , "color" .=? colorToText (sColor st)
  , "duration" .=! sDuration st
  ]

instance ToJSON StateTransition where
  toJSON st = object $ stateTransitionToPairs st

instance FromJSON Status where
  parseJSON (String "ok")        = return Ok
  parseJSON (String "timed_out") = return TimedOut
  parseJSON (String "offline")   = return Offline
  parseJSON (String txt) =
    fail $ "expected status to be 'ok', 'timed_out', or 'offline', but got "
           ++ show txt
  parseJSON _ = fail "expected a JSON string for status"

instance FromJSON Result where
  parseJSON (Object v) = do
    myId     <- v .:  "id"
    myLabel  <- v .:? "label"
    myStatus <- v .:  "status"
    return $ Result myId myLabel myStatus
  parseJSON _ = fail "expected a JSON object for result"

instance FromJSON StateTransitionResult where
  parseJSON (Object v) = do
    myOp      <- v .: "operation"
    myResults <- v .: "results"

    mySelectors       <- myOp .: "selector"
    myStateTransition <- parseJSON (Object myOp)

    return $ StateTransitionResult (unWrapSelectors mySelectors, myStateTransition) myResults

  parseJSON _ = fail "expected a JSON object for state transition result"

parseUuid :: Maybe Object -> Parser (Maybe U.UUID)
parseUuid Nothing = return Nothing
parseUuid (Just v) = do
  myUuidTxt <- v .:? "uuid"
  case myUuidTxt of
   Nothing -> return Nothing
   Just txt -> case U.fromText txt of
                Just x -> return $ Just x
                Nothing -> fail "could not parse uuid as a UUID"

instance FromJSON Scene where
  parseJSON (Object v) = do
    myId      <- v .:  "uuid"
    myName    <- v .:  "name"
    myUpAt    <- v .:  "updated_at"
    myCrAt    <- v .:  "created_at"
    myAccount <- v .:? "account" >>= parseUuid
    myStates  <- v .:  "states"
    return $ Scene myId myName
      (ununix myUpAt) (ununix myCrAt)
      myAccount myStates
  parseJSON _ = fail "expected a JSON object for scene"

instance FromJSON SceneState where
  parseJSON (Object v) = do
    mySel   <- v .: "selector"
    myPower <- v .:? "power"
    myColor <- parseColorBrightness v
    return $ SceneState mySel myPower myColor
  parseJSON _ = fail "expected a JSON object for device in scene"


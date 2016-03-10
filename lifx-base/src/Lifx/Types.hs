{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Lifx.Types where

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Control.Arrow (first)
import Control.Exception
import Control.Monad
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Parser)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as L
import Data.Hourglass
import Data.Int
import Data.List (find, partition)
import Data.Maybe
import Data.Monoid hiding (Product)
import Data.Text (Text(..))
import qualified Data.Text as T
import Data.Text.Buildable
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Text.Format
import Data.Text.Format.Params
import qualified Data.Text.Lazy as LT
import Data.Typeable
import qualified Data.UUID.Types as U
import Data.Version
import Data.Word
import Debug.Trace
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadPrec (readPrec_to_S)
import Text.Read hiding (String)

import Lifx.Util

-- | Exception raised by LIFX functions.
data LifxException =
    -- | Specified LAN interface does not exist.
    --
    -- requested interface name / list of valid interface names
    NoSuchInterface T.Text [T.Text]
    -- | LIFX cloud servers returned the given error message.
  | CloudError T.Text
    -- | Was unable to parse the JSON returned by LIFX cloud servers.
    --
    -- error message / response body
  | CloudJsonError T.Text L.ByteString
    -- | An error occurred making an HTTP request to the cloud.
    --
    -- error message / exception which can be cast to @HttpException@
  | CloudHttpError T.Text SomeException
    -- | The given character is not allowed in a label.
  | IllegalCharacter Char
    -- | The given selector was not found.
  | SelectorNotFound Selector
  deriving (Show, Typeable)

-- have to write this out by hand because SomeException
-- is not an instance of Eq
instance Eq LifxException where
  (NoSuchInterface x y) == (NoSuchInterface x' y') = x == x' && y == y'
  (CloudError x) == (CloudError x') = x == x'
  (CloudJsonError x y) == (CloudJsonError x' y') = x == x' && y == y'
  (CloudHttpError x _ ) == (CloudHttpError x' _ ) = x == x'
  (IllegalCharacter x) == (IllegalCharacter x') = x == x'
  (SelectorNotFound x) == (SelectorNotFound x') = x == x'
  _ == _ = False

instance Exception LifxException

-- | The power state of a bulb.
data Power = Off | On deriving (Show, Read, Eq, Ord)

-- | Color of light, specified as
-- <https://en.wikipedia.org/wiki/HSL_and_HSV hue, saturation, brightness>,
-- and <https://en.wikipedia.org/wiki/Color_temperature kelvin>.
data HSBK a =
  HSBK
  { hue :: a
  , saturation :: a
  , brightness :: a
  , kelvin :: a
  } deriving (Show, Read, Eq, Ord)


instance Functor HSBK where
  fmap f x = HSBK { hue = f $ hue x
                  , saturation = f $ saturation x
                  , brightness = f $ brightness x
                  , kelvin = f $ kelvin x
                  }


-- | Type used for fractional numbers in this package.
type LiFrac = Double

-- | A color specified as floating point hue (0.0 - 360.0),
-- saturation (0.0 - 1.0), brightness (0.0 - 1.0), and kelvin (2500.0 - 9000.0).
type Color = HSBK LiFrac

-- | Same as 'Color', but each of the HSBK components is a 'Maybe', so it's
-- possible to specify a subset of HSBK.
type MaybeColor = HSBK (Maybe LiFrac)

-- https://community.lifx.com/t/comprehensive-list-of-recognized-color-names/1067/2
white, red, orange, yellow, green, cyan, blue, purple, pink :: MaybeColor
white  = HSBK Nothing    (Just 0) Nothing Nothing
red    = HSBK (Just 0)   (Just 1) Nothing Nothing
orange = HSBK (Just 36)  (Just 1) Nothing Nothing
yellow = HSBK (Just 60)  (Just 1) Nothing Nothing
green  = HSBK (Just 120) (Just 1) Nothing Nothing
cyan   = HSBK (Just 180) (Just 1) Nothing Nothing
blue   = HSBK (Just 250) (Just 1) Nothing Nothing
purple = HSBK (Just 280) (Just 1) Nothing Nothing
pink   = HSBK (Just 325) (Just 1) Nothing Nothing

-- | Combines two 'MaybeColor's, so that if either color has 'Just' in a
-- particular component, that value appears in the output.  If both
-- colors have 'Just' in a component, the second color takes precedence
-- over the first for that component.
combineColors :: MaybeColor -> MaybeColor -> MaybeColor
combineColors x y = HSBK
  { hue = hue x `combineMaybe` hue y
  , saturation = saturation x `combineMaybe` saturation y
  , brightness = brightness x `combineMaybe` brightness y
  , kelvin = kelvin x `combineMaybe` kelvin y
  }

combineMaybe :: Maybe a -> Maybe a -> Maybe a
combineMaybe x Nothing = x
combineMaybe _ x@(Just _ ) = x


-- | The 6-byte ID of a single light, which is also its
-- <https://en.wikipedia.org/wiki/MAC_address MAC address>.
newtype DeviceId   = DeviceId B.ByteString   deriving (Eq, Ord)

-- | The 16-byte ID of a group.
newtype GroupId    = GroupId B.ByteString    deriving (Eq, Ord)

-- | The 16-byte ID of a location.
newtype LocationId = LocationId B.ByteString deriving (Eq, Ord)

-- | The human-readable label of a light, group, or location.
-- May be up to 32 bytes of UTF-8 encoded text.
newtype Label      = Label B.ByteString      deriving (Eq, Ord)

-- | The 32-byte
-- <http://api.developer.lifx.com/docs/authentication access token>
-- used by the HTTP API.
newtype AccessToken  = AccessToken B.ByteString  deriving (Eq, Ord)

-- | This class contains methods for encoding and decoding the
-- various ID types.  For most of the ID types, which are binary,
-- the 'T.Text' representation is a base16 string encoding of the
-- 'B.ByteString' representation.  For the 'Label' type, which is text,
-- the 'B.ByteString' representation is the UTF-8 encoding of the
-- 'T.Text' representation, padded with @0@ bytes to be 32 bytes long.
class LifxId t where
  -- | Convert an ID to a 'B.ByteString'.
  toByteString :: t -> B.ByteString
  -- | Create an ID from a 'B.ByteString'.  Returns an error message
  -- if the input is invalid.
  fromByteString :: B.ByteString -> Either String t
  -- | Convert an ID to 'T.Text'.
  toText :: t -> Text
  -- | Create an ID from 'T.Text'.  Returns an error message if the
  -- input is invalid.
  fromText :: Text -> Either String t

checkLength :: String -> Int -> B.ByteString -> Either String B.ByteString
checkLength tname len bs
  | bslen == len = Right bs
  | otherwise =
      Left ("when constructing " ++ tname
            ++ " from ByteString, expected "
            ++ show len ++ " bytes, but got " ++ show bslen)
  where bslen = B.length bs

idToText :: B.ByteString -> Text
idToText bs = TE.decodeUtf8 $ B16.encode bs

textToId :: String -> Int -> Text -> Either String B.ByteString
textToId tname len txt
  | extralen /= 0 =
      Left ("Got crud " ++ show extra ++ " after " ++ tname)
  | bslen == len = Right bs
  | otherwise =
      Left ("when constructing " ++ tname
            ++ " from Text, expected "
            ++ show len ++ " bytes, but got " ++ show bslen)
  where bslen = B.length bs
        (bs, extra) = B16.decode $ TE.encodeUtf8 txt
        extralen = B.length extra

implShow :: B.ByteString -> String -> String
implShow bs post = B8.unpack (B16.encode bs) ++ post

implRead :: (B.ByteString -> a) -> Int -> String -> [(a, String)]
implRead c len s =
  let digs = len * 2
      digs' = digs + 2
      (bs, _ ) = B16.decode (B8.pack $ take digs' s)
  in if B.length bs == len
     then [(c bs, drop digs s)]
     else []

implParseJson :: LifxId a => Value -> Parser a
implParseJson (String txt) = chkSuccess (fromText txt)
  where chkSuccess (Right x) = return x
        chkSuccess (Left x) = fail x
implParseJson _ = fail "expected a JSON string"


deviceIdLen = 6

instance LifxId DeviceId where
  toByteString (DeviceId bs) = bs
  fromByteString bs = DeviceId <$> checkLength "DeviceId" deviceIdLen bs
  toText (DeviceId bs) = idToText bs
  fromText txt = DeviceId <$> textToId "DeviceId" deviceIdLen txt

instance Show DeviceId where
  showsPrec _ (DeviceId bs) post = implShow bs post

instance Read DeviceId where
  readsPrec _ s = implRead DeviceId deviceIdLen s

instance Binary DeviceId where
  put (DeviceId bs) = putByteString bs
  get = DeviceId <$> getByteString deviceIdLen

instance FromJSON DeviceId where
  parseJSON = implParseJson


groupIdLen = 16

instance LifxId GroupId where
  toByteString (GroupId bs) = bs
  fromByteString bs = GroupId <$> checkLength "GroupId" groupIdLen bs
  toText (GroupId bs) = idToText bs
  fromText txt = GroupId <$> textToId "GroupId" groupIdLen txt

instance Show GroupId where
  showsPrec _ (GroupId bs) pre = implShow bs pre

instance Read GroupId where
  readsPrec _ s = implRead GroupId groupIdLen s

instance Binary GroupId where
  put (GroupId bs) = putByteString bs
  get = GroupId <$> getByteString groupIdLen

instance FromJSON GroupId where
  parseJSON = implParseJson


locationIdLen = 16

instance LifxId LocationId where
  toByteString (LocationId bs) = bs
  fromByteString bs = LocationId <$> checkLength "LocationId" locationIdLen bs
  toText (LocationId bs) = idToText bs
  fromText txt = LocationId <$> textToId "LocationId" locationIdLen txt

instance Show LocationId where
  showsPrec _ (LocationId bs) pre = implShow bs pre

instance Read LocationId where
  readsPrec _ s = implRead LocationId locationIdLen s

instance Binary LocationId where
  put (LocationId bs) = putByteString bs
  get = LocationId <$> getByteString locationIdLen

instance FromJSON LocationId where
  parseJSON = implParseJson


labelLen = 32

labelFromText txt = Label $ textToPaddedByteString labelLen txt

instance LifxId Label where
  toByteString (Label bs) = bs
  fromByteString bs = Label <$> checkLength "Label" labelLen bs
  toText (Label bs) =
    TE.decodeUtf8With TEE.lenientDecode $ B.takeWhile (/= 0) bs
  fromText txt = Right $ labelFromText txt

instance Show Label where
  showsPrec p lbl pre = showsPrec p (toText lbl) pre

instance Read Label where
  readsPrec p s = map (first labelFromText) $ readsPrec p s

instance Binary Label where
  put (Label bs) = putByteString bs
  get = Label <$> getByteString labelLen

instance FromJSON Label where
  parseJSON = implParseJson


authTokenLen = 32

instance LifxId AccessToken where
  toByteString (AccessToken bs) = bs
  fromByteString bs = AccessToken <$> checkLength "AccessToken" authTokenLen bs
  toText (AccessToken bs) = idToText bs
  fromText txt = AccessToken <$> textToId "AccessToken" authTokenLen txt

instance Show AccessToken where
  showsPrec _ (AccessToken bs) pre = implShow bs pre

instance Read AccessToken where
  readsPrec _ s = implRead AccessToken authTokenLen s

instance Binary AccessToken where
  put (AccessToken bs) = putByteString bs
  get = AccessToken <$> getByteString authTokenLen

instance FromJSON AccessToken where
  parseJSON = implParseJson


-- | Represents a <http://api.developer.lifx.com/docs/selectors selector>
-- for addressing a set of lights.
data Selector = SelAll
              | SelLabel Label
              | SelDevId DeviceId
              | SelGroup Label
              | SelGroupId GroupId
              | SelLocation Label
              | SelLocationId LocationId
              | SelSceneId SceneId
                deriving (Show, Read, Eq, Ord)


-- | A 'MaybeColor' where all components are 'Nothing'.
emptyColor :: MaybeColor
emptyColor = HSBK Nothing Nothing Nothing Nothing

-- | Are all components of this 'MaybeColor' 'Nothing'?
isEmptyColor (HSBK Nothing Nothing Nothing Nothing) = True
isEmptyColor _ = False

-- | Are all components of this 'MaybeColor' 'Just'?
isCompleteColor (HSBK (Just _ ) (Just _ ) (Just _ ) (Just _ )) = True
isCompleteColor _ = False


-- | Specifies what a bulb is capable of.
data Capabilities =
  Capabilities
  { cHasColor             :: !Bool
  , cHasVariableColorTemp :: !Bool
  } deriving (Show, Read, Eq, Ord)

-- | Information about a particular model of bulb.
data Product =
  Product
  { -- | Human-readable name of the company that made this bulb,
    -- such as \"LIFX\".
    pCompanyName  :: Text
    -- | Human-readable name of the model of this bulb, as as \"Original 1000\".
  , pProductName  :: Text
    -- | A slightly-less-human-readable string which identifies this model,
    -- such as \"lifx_original_a21\".
  , pIdentifier   :: Text
    -- | Information about whether this bulb supports full color,
    -- or a variable color temperature of white.
  , pCapabilities :: Capabilities
  } deriving (Show, Read, Eq, Ord)


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
  -- next 'StateTransition' in the list, wrapping around to the
  -- beginning if necessary.  Corresponds to
  -- <http://api.developer.lifx.com/docs/cycle Cycle> endpoint.
  cycleLights :: t                    -- ^ The connection.
                 -> [Selector]        -- ^ The lights to operate on.
                 -> [StateTransition] -- ^ States to cycle through
                 -> IO [Result]

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

-- | An amount of time, specified as a floating-point number of seconds.
type FracSeconds = Double

-- | Hints about what information is needed from 'listLights'.
data InfoNeeded = NeedLabel | NeedPower | NeedColor | NeedGroup | NeedLocation
                | NeedProduct | NeedTemperature | NeedUptime
                | NeedFirmwareVersion | NeedHardwareVersion
                deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | A list of all possible values of 'InfoNeeded', thus requesting as much
-- information as possible be returned.
needEverything :: [InfoNeeded]
needEverything = [minBound .. maxBound]

-- | Information about a light, returned by 'listLights'.
data LightInfo =
  LightInfo
  { lId :: DeviceId
  , lUuid :: Maybe U.UUID
  , lLabel :: Maybe Label
  , lConnected :: Bool
  , lPower :: Maybe Power
  , lColor :: MaybeColor
  , lGroupId :: Maybe GroupId
  , lGroup :: Maybe Label
  , lLocationId :: Maybe LocationId
  , lLocation :: Maybe Label
  , lLastSeen :: DateTime
  , lSecondsSinceSeen :: FracSeconds
  , lProduct :: Maybe Product
  , lTemperature :: Maybe Double
  , lUptime :: Maybe FracSeconds
  , lFirmwareVersion :: Maybe Version
  , lHardwareVersion :: Maybe Int
  } deriving (Eq, Ord, Show, Read)


data StateTransition =
  StateTransition
  { sPower :: Maybe Power
  , sColor :: MaybeColor
  , sDuration :: FracSeconds
  } deriving (Eq, Ord, Show, Read)

data Result =
  Result
  { rId :: DeviceId
  , rLabel :: Maybe Label
  , rStatus :: Status
  } deriving (Eq, Ord, Show, Read)

data Status = Ok | TimedOut | Offline deriving (Eq, Ord, Show, Read)

data StateTransitionResult =
  StateTransitionResult
  { tOperation :: ([Selector], StateTransition)
  , tResults :: [Result]
  } deriving (Eq, Ord, Show, Read)

-- | The shape of the waveform of an 'Effect'.
data EffectType = Pulse   -- ^ a square wave
                | Breathe -- ^ a sine wave
                  deriving (Eq, Ord, Show, Read)

-- | Specifies details of the effect performed by 'effect'.
data Effect =
  Effect
  { eType :: EffectType      -- ^ The shape of the waveform.  Default 'Pulse'.
  , eColor :: MaybeColor     -- ^ The color of the effect.
  , eFromColor :: MaybeColor -- ^ The color to start from.  'emptyColor'
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
  , ssColor :: MaybeColor
  } deriving (Eq, Ord, Show, Read)

-- | Scenes are uniquely identified by a 'U.UUID'.
newtype SceneId = SceneId { unSceneId :: U.UUID } deriving (Eq, Ord)

instance Show SceneId where
  showsPrec prec sid = showsPrec prec (unSceneId sid)

instance Read SceneId where
  readsPrec prec str = map (first SceneId) (readsPrec prec str)

sceneFromUuid :: Maybe U.UUID -> Either String SceneId
sceneFromUuid u = maybe (Left "Can't parse UUID") (Right . SceneId) u

instance LifxId SceneId where
  toByteString (SceneId uu) = L.toStrict $ U.toByteString uu
  fromByteString bs = sceneFromUuid $ U.fromByteString $ L.fromStrict bs
  toText (SceneId uu) = U.toText uu
  fromText bs = sceneFromUuid $ U.fromText bs

instance FromJSON SceneId where
  parseJSON = implParseJson

textualize :: Buildable a => (T.Text, Maybe a) -> Maybe T.Text
textualize (_, Nothing) = Nothing
textualize (key, Just value) = Just $ fmt "{}:{}" (key, value)

-- | When given 'emptyColor', returns 'Nothing'.  Otherwise, returns
-- 'Just' a non-empty string which, when fed to 'Lifx.parseColor', would
-- parse as the given color.
colorToText :: MaybeColor -> Maybe T.Text
colorToText c@(HSBK h s b k)
  | isEmptyColor c = Nothing
  | otherwise =
      -- kelvin has to go before saturation because of weird behavior:
      -- https://community.lifx.com/t/interpolating-colors-whites/573/8
      -- Also, kelvin has to be an Int and not a Double like the others,
      -- or else we get CloudError "Unable to parse color: kelvin:5000.00"
      let k' = ("kelvin", fmap round k :: Maybe Int)
          hsb = zip ["hue", "saturation", "brightness"] [h, s, b]
          components1 = mapMaybe textualize [k']
          components2 = mapMaybe textualize hsb
      in Just $ T.intercalate " " (components1 ++ components2)

-- | Renders a 'Selector' in
-- <http://api.developer.lifx.com/docs/selectors the same format> accepted
-- by the LIFX Cloud API.
selectorToText :: Selector -> T.Text
selectorToText SelAll = "all"
selectorToText (SelLabel x)      = "label:"       <> toText x
selectorToText (SelDevId x)      = "id:"          <> toText x
selectorToText (SelGroup x)      = "group:"       <> toText x
selectorToText (SelGroupId x)    = "group_id:"    <> toText x
selectorToText (SelLocation x)   = "location:"    <> toText x
selectorToText (SelLocationId x) = "location_id:" <> toText x
selectorToText (SelSceneId x)    = "scene_id:"    <> toText x

-- | Renders a list of 'Selector's as a comma-separated string.
selectorsToText :: [Selector] -> Either LifxException T.Text
selectorsToText sels = do
  let illegalChars = ",/"
      checkChars t = checkChars' t $ T.find (`elem` illegalChars) t
      checkChars' t Nothing = Right t
      checkChars' _ (Just c) = Left $ IllegalCharacter c
  txts <- mapM (checkChars . selectorToText) sels
  return $ T.intercalate "," txts

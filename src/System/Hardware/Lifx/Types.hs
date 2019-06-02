{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module System.Hardware.Lifx.Types where

import Control.Applicative ((<$>),(<|>))
import Control.Arrow (first)
import Control.Exception
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Parser)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Char8 as B8
-- import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as L
import Data.Text (Text(..))
import qualified Data.Text as T
-- import qualified Data.Text.Encoding as TE
-- import qualified Data.Text.Encoding.Error as TEE
import Data.Typeable
import qualified Data.UUID.Types as U

import System.Hardware.Lifx.Lan.LowLevel
import System.Hardware.Lifx.Lan.LowLevel.Internal
-- import System.Hardware.Lifx.Util

-- | Exception raised by LIFX functions.
data LifxException =
    -- | Specified LAN interface does not exist.
    --
    -- requested interface name / list of valid interface names
    NoSuchInterface Interface [Interface]
    -- | No access token was specified in the config file.
    --
    -- name of config file
  | NoAccessToken FilePath
    -- | LIFX cloud servers returned the given error message.
  | RemoteError T.Text
    -- | Was unable to parse the JSON returned by LIFX cloud servers.
    --
    -- error message / response body
  | JsonError T.Text L.ByteString
    -- | An error occurred making an HTTP request to the cloud.
    --
    -- error message / exception which can be cast to @HttpException@
  | HttpError T.Text SomeException
    -- | An error occurred making an HTTP request to the cloud.
    --
    -- http status code / error message
  | HttpStatusError Int T.Text
    -- | The given character is not allowed in a label.
  | IllegalCharacter Char
    -- | The given selector was not found.
    -- | A scene ID was used as a selector, and that scene contained a
    -- selector which was also a scene ID.
    --
    -- original scene ID / nested scene ID
  | NestedSceneIdSelector SceneId SceneId
    -- | The value for a parameter was out of range, or otherwise
    -- incorrect.
  | BadParam ParamError
    -- | The backend does not support something you asked it to do.
  | Unsupported T.Text
  deriving (Show, Typeable)

-- have to write this out by hand because SomeException (in HttpError)
-- is not an instance of Eq
instance Eq LifxException where
  (NoSuchInterface x y) == (NoSuchInterface x' y') = x == x' && y == y'
  (NoAccessToken x) == (NoAccessToken x') = x == x'
  (RemoteError x) == (RemoteError x') = x == x'
  (JsonError x y) == (JsonError x' y') = x == x' && y == y'
  (HttpError x _ ) == (HttpError x' _ ) = x == x'
  (IllegalCharacter x) == (IllegalCharacter x') = x == x'
  (NestedSceneIdSelector x y) == (NestedSceneIdSelector x' y') = x == x' && y == y'
  (BadParam x) == (BadParam x') = x == x'
  (Unsupported x) == (Unsupported x') = x == x'
  _ == _ = False

instance Exception LifxException

-- | More detailed information about a 'BadParam' exception.
data ParamError =
    -- | No information is provided except the name of the parameter.
    InvalidParam
    { peName :: T.Text      -- ^ Name of parameter
    }
    -- | The parameter is out of range.
  | InvalidRange
    { peName :: T.Text      -- ^ Name of parameter
    , peMin :: ColorChannel -- ^ Minimum acceptable value for the parameter
    , peMax :: ColorChannel -- ^ Maximum acceptable value for the parameter
    }
    -- | A list parameter has too many or too few entries.
  | InvalidEntries
    { peName :: T.Text      -- ^ Name of parameter
    , peMinEntries:: Int    -- ^ Minimum acceptable number of items in list
    , peMaxEntries :: Int   -- ^ Maximum acceptable number of items in list
    }
    deriving (Eq, Ord, Show, Read)

-- | An amount of time, specified as a floating-point number of seconds.
type FracSeconds = Double


instance Functor HSBK where
  fmap f x = HSBK { hue = f $ hue x
                  , saturation = f $ saturation x
                  , brightness = f $ brightness x
                  , kelvin = f $ kelvin x
                  }


-- | One channel of a color.
type ColorChannel = Double

-- | A color specified as floating point hue (0.0 - 360.0),
-- saturation (0.0 - 1.0), brightness (0.0 - 1.0), and kelvin (2500.0 - 9000.0).
type Color = HSBK ColorChannel

-- | Same as 'Color', but each of the HSBK components is a 'Maybe', so it's
-- possible to specify a subset of HSBK.
type PartialColor = HSBK (Maybe ColorChannel)

minKelvin, maxKelvin :: ColorChannel
minKelvin = 2500
maxKelvin = 9000

-- https://community.lifx.com/t/comprehensive-list-of-recognized-color-names/1067/2
white, red, orange, yellow, green, cyan, blue, purple, pink :: PartialColor
white  = HSBK Nothing    (Just 0) Nothing Nothing
red    = HSBK (Just 0)   (Just 1) Nothing Nothing
orange = HSBK (Just 36)  (Just 1) Nothing Nothing
yellow = HSBK (Just 60)  (Just 1) Nothing Nothing
green  = HSBK (Just 120) (Just 1) Nothing Nothing
cyan   = HSBK (Just 180) (Just 1) Nothing Nothing
blue   = HSBK (Just 250) (Just 1) Nothing Nothing
purple = HSBK (Just 280) (Just 1) Nothing Nothing
pink   = HSBK (Just 325) (Just 1) Nothing Nothing

-- | Combines two 'PartialColor's, so that if either color has 'Just' in a
-- particular component, that value appears in the output.  If both
-- colors have 'Just' in a component, the second color takes precedence
-- over the first for that component.
combineColors :: PartialColor -> PartialColor -> PartialColor
combineColors x y = HSBK
  { hue = hue x <|> hue y
  , saturation = saturation x <|> saturation y
  , brightness = brightness x <|> brightness y
  , kelvin = kelvin x <|> kelvin y
  }


-- | The 32-byte
-- <http://api.developer.lifx.com/docs/authentication access token>
-- used by the HTTP API.
newtype AccessToken  = AccessToken B.ByteString  deriving (Eq, Ord)

implParseJson :: LifxId a => Value -> Parser a
implParseJson (String txt) = chkSuccess (fromText txt)
  where chkSuccess (Right x) = return x
        chkSuccess (Left x) = fail x
implParseJson _ = fail "expected a JSON string"


instance FromJSON DeviceId where
  parseJSON = implParseJson


instance FromJSON GroupId where
  parseJSON = implParseJson


instance FromJSON LocationId where
  parseJSON = implParseJson


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



-- | A 'PartialColor' where all components are 'Nothing'.
emptyColor :: PartialColor
emptyColor = HSBK Nothing Nothing Nothing Nothing

-- | Are all components of this 'PartialColor' 'Nothing'?
isEmptyColor (HSBK Nothing Nothing Nothing Nothing) = True
isEmptyColor _ = False

-- | Are all components of this 'PartialColor' 'Just'?
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

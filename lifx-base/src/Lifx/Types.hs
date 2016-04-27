{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

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
-- import Data.Hourglass
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
-- import Data.Version
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
    -- | The given character is not allowed in a label.
  | IllegalCharacter Char
    -- | The given selector was not found.
  | SelectorNotFound Selector
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
  (SelectorNotFound x) == (SelectorNotFound x') = x == x'
  (NestedSceneIdSelector x y) == (NestedSceneIdSelector x' y') = x == x' && y == y'
  (BadParam x) == (BadParam x') = x == x'
  (Unsupported x) == (Unsupported x') = x == x'
  _ == _ = False

instance Exception LifxException

-- | More detailed information about a 'BadParam' exception.
data ParamError =
    -- | No information is provided except the name of the parameter.
    InvalidParam   { peName :: T.Text }
    -- | The parameter is out of range.
  | InvalidRange   { peName :: T.Text, peMin :: ColorChannel, peMax :: ColorChannel }
    -- | A list parameter has too many or too few entries.
  | InvalidEntries { peName :: T.Text, peMinEntries:: Int, peMaxEntries :: Int }
    deriving (Eq, Ord, Show, Read)

-- | The name of a network interface, such as @en1@ or @eth0@.
type Interface = T.Text

-- | An amount of time, specified as a floating-point number of seconds.
type FracSeconds = Double

-- | The power state of a bulb.
data Power = Off | On deriving (Show, Read, Eq, Ord, Bounded, Enum)

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

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
import Data.List (find)
import Data.Maybe
import Data.Monoid hiding (Product)
import qualified Data.Set as S
import Data.Text (Text(..))
import qualified Data.Text as T
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

data LifxException = NoSuchInterface String [String]
                   | CloudError T.Text
                   | CloudJsonError T.Text
                   | IllegalCharacter Char
                   deriving (Show, Typeable)

instance Exception LifxException

data Power = Off | On deriving (Show, Read, Eq, Ord)

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


type LiFrac = Double

type Color = HSBK LiFrac
type MaybeColor = HSBK (Maybe LiFrac)

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


newtype DeviceId   = DeviceId B.ByteString   deriving (Eq, Ord)
newtype GroupId    = GroupId B.ByteString    deriving (Eq, Ord)
newtype LocationId = LocationId B.ByteString deriving (Eq, Ord)
newtype Label      = Label B.ByteString      deriving (Eq, Ord)
newtype AuthToken  = AuthToken B.ByteString  deriving (Eq, Ord)

class LifxId t where
  toByteString :: t -> B.ByteString
  fromByteString :: B.ByteString -> Either String t
  toText :: t -> Text
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

instance LifxId AuthToken where
  toByteString (AuthToken bs) = bs
  fromByteString bs = AuthToken <$> checkLength "AuthToken" authTokenLen bs
  toText (AuthToken bs) = idToText bs
  fromText txt = AuthToken <$> textToId "AuthToken" authTokenLen txt

instance Show AuthToken where
  showsPrec _ (AuthToken bs) pre = implShow bs pre

instance Read AuthToken where
  readsPrec _ s = implRead AuthToken authTokenLen s

instance Binary AuthToken where
  put (AuthToken bs) = putByteString bs
  get = AuthToken <$> getByteString authTokenLen

instance FromJSON AuthToken where
  parseJSON = implParseJson


data Selector = SelAll
              | SelLabel Label
              | SelDevId DeviceId
              | SelGroup Label
              | SelGroupId GroupId
              | SelLocation Label
              | SelLocationId LocationId
                deriving (Show, Read, Eq, Ord)


emptyColor = HSBK Nothing Nothing Nothing Nothing

isEmptyColor (HSBK Nothing Nothing Nothing Nothing) = True
isEmptyColor _ = False

isCompleteColor (HSBK (Just _ ) (Just _ ) (Just _ ) (Just _ )) = True
isCompleteColor _ = False


data Capabilities =
  Capabilities
  { cHasColor             :: !Bool
  , cHasVariableColorTemp :: !Bool
  } deriving (Show, Read, Eq, Ord)

data Product =
  Product
  { pCompanyName  :: Text
  , pLongName     :: Text
  , pShortName    :: Text
  , pIdentifier   :: Text
  , pCapabilities :: Capabilities
  } deriving (Show, Read, Eq, Ord)


data Targets = TargAll | TargSome (S.Set TargetMatch)
               deriving (Show, Read, Eq, Ord)

data TargetMatch = TmLabel      T.Text
                 | TmDevId      T.Text
                 | TmGroup      T.Text
                 | TmGroupId    T.Text
                 | TmLocation   T.Text
                 | TmLocationId T.Text
                   deriving (Show, Read, Eq, Ord)

data LiteIds =
  LiteIds
  { liDevId    :: DeviceId
  , liLabel    :: Maybe Label
  , liGroupId  :: Maybe GroupId
  , liGroup    :: Maybe Label
  , liLocId    :: Maybe LocationId
  , liLoc      :: Maybe Label
  } deriving (Show, Read, Eq, Ord)

mkLiteIds :: DeviceId -> LiteIds
mkLiteIds d = LiteIds
  { liDevId   = d
  , liLabel   = Nothing
  , liGroupId = Nothing
  , liGroup   = Nothing
  , liLocId   = Nothing
  , liLoc     = Nothing
  }

tmatch :: TargetMatch -> LiteIds -> Bool
tmatch (TmDevId t)      (LiteIds { liDevId   =       x  }) = matchId  t x
tmatch (TmLabel t)      (LiteIds { liLabel   = (Just x) }) = matchLab t x
tmatch (TmGroupId t)    (LiteIds { liGroupId = (Just x) }) = matchId  t x
tmatch (TmGroup t)      (LiteIds { liGroup   = (Just x) }) = matchLab t x
tmatch (TmLocationId t) (LiteIds { liLocId   = (Just x) }) = matchId  t x
tmatch (TmLocation t)   (LiteIds { liLoc     = (Just x) }) = matchLab t x
tmatch _ _ = False

matchId :: LifxId i => T.Text -> i -> Bool
matchId t1 x = t1 `T.isSuffixOf` t2
  where t2 = toText x

matchLab :: T.Text -> Label -> Bool;
matchLab t1 x = t1 `T.isPrefixOf` t2
  where t2 = toText x

----------------------------------------------------------------------

class Connection t where
  listLights :: t -> [Selector] -> [InfoNeeded] -> IO [LightInfo]
  setStates :: t -> [([Selector], StateTransition)] -> IO [StateTransitionResult]
  togglePower :: t -> [Selector] -> FracSeconds -> IO [Result]
  effect :: t -> [Selector] -> Effect -> IO [Result]
  listScenes :: t -> IO [Scene]
  activateScene :: t -> SceneId -> FracSeconds -> IO [Result]
  cycleLights :: t -> [Selector] -> [StateTransition] -> IO [Result]
  closeConnection :: t -> IO ()

{-
selectAll        :: Selector
selectLabel      :: Label      -> Selector
selectDeviceId   :: DeviceId   -> Selector
selectGroup      :: Label      -> Selector
selectGroupId    :: GroupId    -> Selector
selectLocation   :: Label      -> Selector
selectLocationId :: LocationId -> Selector

selectAll        = SelAll
selectLabel      = SelLabel
selectDeviceId   = SelDevId
selectGroup      = SelGroup
selectGroupId    = SelGroupId
selectLocation   = SelLocation
selectLocationId = SelLocationId
-}

type FracSeconds = Double

data InfoNeeded = NeedLabel | NeedPower | NeedColor | NeedGroup | NeedLocation
                | NeedProduct | NeedTemperature | NeedUptime
                | NeedFirmwareVersion | NeedHardwareVersion
                deriving (Show, Read, Eq, Ord, Bounded, Enum)

needEverything :: [InfoNeeded]
needEverything = [minBound .. maxBound]

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
  { tOperation :: (Selector, StateTransition)
  , tResults :: [Result]
  } deriving (Eq, Ord, Show, Read)

data EffectType = Pulse | Breathe deriving (Eq, Ord, Show, Read)

data Effect =
  Effect
  { eType :: EffectType
  , eColor :: MaybeColor
  , eFromColor :: MaybeColor
  , ePeriod :: FracSeconds
  , eCycles :: Double
  , ePersist :: Bool
  , ePowerOn :: Bool
  , ePeak :: Double
  } deriving (Eq, Ord, Show, Read)

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

newtype SceneId = SceneId { unSceneId :: U.UUID }
                deriving (Eq, Ord, Show, Read {- , Hashable -})

sceneFromUuid :: Maybe U.UUID -> Either String SceneId
sceneFromUuid u = maybe (Left "Can't parse UUID") (Right . SceneId) u

instance LifxId SceneId where
  toByteString (SceneId uu) = L.toStrict $ U.toByteString uu
  fromByteString bs = sceneFromUuid $ U.fromByteString $ L.fromStrict bs
  toText (SceneId uu) = U.toText uu
  fromText bs = sceneFromUuid $ U.fromText bs

instance FromJSON SceneId where
  parseJSON = implParseJson

textualize :: (T.Text, Maybe Double) -> Maybe T.Text
textualize (_, Nothing) = Nothing
textualize (key, Just value) = Just $ fmt "{}:{}" (key, value)

colorToText :: MaybeColor -> Maybe T.Text
colorToText c@(HSBK h s b k)
  | isEmptyColor c = Nothing
  | otherwise =
      -- kelvin has to go before saturation because of weird behavior:
      -- https://community.lifx.com/t/interpolating-colors-whites/573/8
      let hsbk = zip ["kelvin", "hue", "saturation", "brightness"] [k, h, s, b]
          components = mapMaybe textualize hsbk
      in Just $ T.intercalate " " components

selectorToText :: Selector -> T.Text
selectorToText SelAll = "all"
selectorToText (SelLabel x)      = "label:"       <> toText x
selectorToText (SelDevId x)      = "id:"          <> toText x
selectorToText (SelGroup x)      = "group:"       <> toText x
selectorToText (SelGroupId x)    = "group_id:"    <> toText x
selectorToText (SelLocation x)   = "location:"    <> toText x
selectorToText (SelLocationId x) = "location_id:" <> toText x

selectorsToText :: [Selector] -> Either LifxException T.Text
selectorsToText sels = do
  let illegalChars = ",/"
      checkChars t = checkChars' t $ T.find (`elem` illegalChars) t
      checkChars' t Nothing = Right t
      checkChars' _ (Just c) = Left $ IllegalCharacter c
  txts <- mapM (checkChars . selectorToText) sels
  return $ T.intercalate "," txts

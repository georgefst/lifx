{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Lifx.Types
       ( Power (..)
       , HSBK (..)
       , LiFrac , Color , MaybeColor
       , white, red, orange, yellow, green, cyan, blue, purple, pink
       , combineColors, emptyColor , isEmptyColor, isCompleteColor
       , DeviceId, GroupId, LocationId, Label, AuthToken
       , LifxId (..)
       , Product (..)
       , products, productFromId, productFromLongName, productFromShortName
       , Targets (..)
       , TargetMatch (..)
       , LiteIds (..)
       , mkLiteIds
       , tmatch
       , padByteString
       , readEither'
       , Selector
       , selectAll, selectLabel, selectDeviceId
       , selectGroup, selectGroupId, selectLocation, selectLocationId
       , Connection (..)
       , FracSeconds
       , LightInfo (..)
       , Capabilities (..)
       , StateTransition (..)
       , Result (..)
       , Status (..)
       , StateTransitionResult (..)
       , EffectType (..)
       , Effect (..)
       , Scene (..)
       , SceneDevice (..)
       , SceneId (..)
       , colorToText
       ) where

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Control.Arrow (first)
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
import Data.List (find)
import Data.Maybe
import Data.Monoid (Monoid(..))
import qualified Data.Set as S
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Text.Format
import Data.Text.Format.Params
import qualified Data.Text.Lazy as LT
import qualified Data.UUID.Types as U
import Data.Version
import Data.Word
import Debug.Trace
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadPrec (readPrec_to_S)
import Text.Read hiding (String)

data Power = Off | On deriving (Show, Read, Eq, Ord)

instance FromJSON Power where
  parseJSON (String "on") = return On
  parseJSON (String "off") = return Off
  parseJSON _ = fail "could not parse power"

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
implShow bs pre = pre ++ B8.unpack (B16.encode bs)

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
  showsPrec _ (DeviceId bs) pre = implShow bs pre

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


data Capabilities = HasColor | HasVariableColorTemp
                  deriving (Show, Read, Eq, Ord)

data Product =
  Product
  { pVendor       :: !Word32
  , pProduct      :: !Word32
  , pLongName     :: Text
  , pShortName    :: Text
  , pCapabilities :: [Capabilities]
  } deriving (Show, Read, Eq, Ord)

products :: [Product]
products =
  [ Product 1 1 "LIFX Original 1000" "O1000" [HasColor, HasVariableColorTemp]
  , Product 1 2 "LIFX Color 650"     "C650"  [HasColor, HasVariableColorTemp]
  , Product 1 3 "LIFX White 800"     "W800"  [HasVariableColorTemp]
  ]

productFromId :: Word32 -> Word32 -> Maybe Product
productFromId v p = find f products
  where f (Product v' p' _ _ _) = v == v' && p == p'

productFromLongName :: Text -> Maybe Product
productFromLongName ln = find f products
  where f (Product _ _ ln' _ _) = ln == ln'

productFromShortName :: Text -> Maybe Product
productFromShortName sn = find f products
  where f (Product _ _ _ sn' _) = sn == sn'

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

---- Utilities: move elsewhere?

-- readEither has been in Text.Read since base 4.6,
-- but we have our own copy here to work with base 4.5.
-- BSD3, (c) The University of Glasgow 2001
readEither' :: Read a => String -> Either String a
readEither' s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift skipSpaces
       return x

fmt :: Params ps => Format -> ps -> T.Text
fmt f p = LT.toStrict $ format f p

-- pad (with 0) or truncate a bytestring to make it exactly the specified length
padByteString :: Int -> B.ByteString -> B.ByteString
padByteString goal bs = f (l `compare` goal)
  where l = B.length bs
        f LT = bs `B.append` pad
        f EQ = bs
        f GT = B.take goal bs
        pad = B.replicate (goal - l) 0

-- truncate a Text to fit in the specific number of bytes, encoded as UTF-8,
-- but without truncating in the middle of a character
textToByteString :: Int -> T.Text -> B.ByteString
textToByteString maxBytes txt = t2bs (maxBytes `div` 4)
  where t2bs n =
          let nPlus1 = n + 1
              bs = convert nPlus1
              bsLen = B.length bs
          in if bsLen > maxBytes
             then convert n
             else if bsLen == maxBytes || n >= txtLen
                  then bs
                  else t2bs nPlus1
        txtLen = T.length txt
        convert n = TE.encodeUtf8 $ T.take n txt

textToPaddedByteString :: Int -> T.Text -> B.ByteString
textToPaddedByteString maxBytes txt =
  padByteString maxBytes $ textToByteString maxBytes txt

----------------------------------------------------------------------

class Connection t where
  listLights :: t -> Selector -> IO [LightInfo]
  setStates :: t -> [(Selector, StateTransition)] -> IO [StateTransitionResult]
  effect :: t -> Selector -> Effect -> IO [Result]
  listScenes :: t -> IO [Scene]
  activateScene :: t -> SceneId -> FracSeconds -> IO [Result]
  cycle :: t -> Selector -> [StateTransition] -> IO [Result]
  terminate :: t -> IO ()

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

type FracSeconds = Double

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

parseIdStruct :: FromJSON a => Maybe Value -> Parser (Maybe a, Maybe Label)
parseIdStruct (Just (Object v)) = do
  i <- v .:? "id"
  n <- v .:? "name"
  return (i, n)
parseIdStruct _ = return (Nothing, Nothing)

combineColorBrightness :: Maybe Value -> Maybe Double -> Parser MaybeColor
combineColorBrightness c b = do
  c' <- parseColor c
  return $ addBrightness c' b
  where addBrightness cc Nothing = cc
        addBrightness cc br@(Just _ ) = cc { brightness = br }
        parseColor Nothing = return emptyColor
        parseColor (Just (Object v)) = do
          myHue        <- v .:? "hue"
          mySaturation <- v .:? "saturation"
          myBrightness <- v .:? "brightness"
          myKelvin     <- v .:? "kelvin"
          return $ HSBK myHue mySaturation myBrightness myKelvin
        parseColor _ = fail "expected a JSON object"

parseCaps :: Value -> Parser [Capabilities]
parseCaps (Object v) = do
  hasColor  <- v .: "has_color"
  hasVCTemp <- v .: "has_variable_color_temp"
  let hc   = if hasColor  then [HasColor]             else []
      hvct = if hasVCTemp then [HasVariableColorTemp] else []
  return $ hc ++ hvct
parseCaps _ = fail "expected a JSON object"

combineProd :: T.Text -> Maybe [Capabilities] -> Product
combineProd pname (Just caps) =
  (combineProd pname Nothing) { pCapabilities = caps }
combineProd pname Nothing = mkProd $ productFromLongName pname
  where mkProd (Just p) = p
        mkProd Nothing = Product
                         { pVendor = 0
                         , pProduct = 0
                         , pLongName = pname
                         , pShortName = pname
                         , pCapabilities = []
                         }

instance FromJSON LightInfo where
  parseJSON (Object v) = do
    myId               <- v .:  "id"
    myUuidTxt          <- v .:? "uuid"
    myLabel            <- v .:? "label"
    myConnected        <- v .:  "connected"
    myPower            <- v .:? "power"
    myColorObj         <- v .:? "color"
    myBrightness       <- v .:? "brightness"
    myGroupStruct      <- v .:? "group"
    myLocationStruct   <- v .:? "location"
    myLastSeenStr      <- v .:  "last_seen"
    mySecondsSinceSeen <- v .:  "seconds_since_seen"

    myProductName      <- v .:? "product_name"
    myCapabilities     <- v .:? "capabilities"
    myFirmwareVersStr  <- v .:? "firmware_version"
    myHardwareVersion  <- v .:? "hardware_version"

    myTemperature      <- v .:? "temperature"
    myUptime           <- v .:? "uptime"

    (myGroupId, myGroup)       <- parseIdStruct myGroupStruct
    (myLocationId, myLocation) <- parseIdStruct myLocationStruct

    myLastSeen <- case timeParse ISO8601_DateAndTime myLastSeenStr of
                   Just x -> return x
                   Nothing -> fail "could not parse last_seen as ISO8601 date"

    myUuid <- case myUuidTxt of
               Nothing -> return Nothing
               Just txt -> case U.fromText txt of
                            Just x -> return $ Just x
                            Nothing -> fail "could not parse uuid as a UUID"

    myColor <- combineColorBrightness myColorObj myBrightness

    myFirmwareVersion <- case myFirmwareVersStr of
                          Nothing -> return Nothing
                          Just s -> case readEither' s of
                                     Left msg -> fail msg
                                     Right vers -> return $ Just vers

    myCaps <- case myCapabilities of
               Nothing -> return Nothing
               Just obj -> Just <$> parseCaps obj

    let myProduct = case myProductName of
                     Nothing -> Nothing
                     Just p -> Just $ combineProd p myCaps

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
           , lProduct          = myProduct
           , lTemperature      = myTemperature
           , lUptime           = myUptime
           , lFirmwareVersion  = myFirmwareVersion
           , lHardwareVersion  = myHardwareVersion
           }

  parseJSON _ = fail "expected a JSON object"

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

data Scene =
  Scene
  { scUpdatedAt :: DateTime
  , scCreatedAt :: DateTime
  , scDevices :: [SceneDevice]
  , scAccount :: Maybe U.UUID
  , scName :: T.Text
  , scId :: SceneId
  } deriving (Eq, Ord, Show, Read)

data SceneDevice =
  SceneDevice
  { sdId :: DeviceId
  , sdPower :: Maybe Power
  , sdColor :: MaybeColor
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

textualize :: (T.Text, Maybe Double) -> Maybe T.Text
textualize (_, Nothing) = Nothing
textualize (key, Just value) = Just $ fmt "{}:{}" (key, value)

colorToText :: MaybeColor -> T.Text
colorToText (HSBK h s b k) =
  let hsbk = zip ["hue", "saturation", "brightness", "kelvin"] [h, s, b, k]
      components = mapMaybe textualize hsbk
  in T.intercalate " " components

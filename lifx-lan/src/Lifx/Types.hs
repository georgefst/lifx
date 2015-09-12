{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Lifx.Types where

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Control.Arrow (first)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
-- import Data.Hashable
import Data.List (find)
import Data.Monoid (Monoid(..))
import qualified Data.Set as S
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Word
import Debug.Trace

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


instance (Monoid a) => Monoid (HSBK a) where
  mempty = HSBK { hue = mempty
                , saturation = mempty
                , brightness = mempty
                , kelvin = mempty
                }

  mappend x y = HSBK { hue = hue x `mappend` hue y
                     , saturation = saturation x `mappend` saturation y
                     , brightness = brightness x `mappend` brightness y
                     , kelvin = kelvin x `mappend` kelvin y
                     }

type LiFrac = Double
type Interface = Text

type HSBK16 = HSBK Word16
type Color = HSBK LiFrac
type MaybeColor = HSBK (Maybe LiFrac)

white, red, orange, yellow, cyan, green, blue, purple, pink :: MaybeColor
white  = HSBK Nothing    (Just 0) Nothing Nothing
red    = HSBK (Just 0)   (Just 1) Nothing Nothing
orange = HSBK (Just 36)  (Just 1) Nothing Nothing
yellow = HSBK (Just 60)  (Just 1) Nothing Nothing
cyan   = HSBK (Just 180) (Just 1) Nothing Nothing
green  = HSBK (Just 120) (Just 1) Nothing Nothing
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


authTokenLen = 32

instance LifxId AuthToken where
  toByteString (AuthToken bs) = bs
  fromByteString bs = AuthToken <$> checkLength "AuthToken" locationIdLen bs
  toText (AuthToken bs) = idToText bs
  fromText txt = AuthToken <$> textToId "AuthToken" locationIdLen txt

instance Show AuthToken where
  showsPrec _ (AuthToken bs) pre = implShow bs pre

instance Read AuthToken where
  readsPrec _ s = implRead AuthToken locationIdLen s

instance Binary AuthToken where
  put (AuthToken bs) = putByteString bs
  get = AuthToken <$> getByteString locationIdLen


data Selectors = SelAll
               | SelSome (S.Set Selector)

data Selector = SelLabel Label
              | SelDevId DeviceId
              | SelGroup Label
              | SelGroupId GroupId
              | SelLocation Label
              | SelLocationId LocationId
                deriving (Show, Read, Eq, Ord)

{-
data ColorArg = CNamed  NamedColor
              | CCustom MaybeColor
                deriving (Show, Eq, Ord)

data NamedColor = White | Red | Orange | Yellow
                | Cyan | Green | Blue | Purple | Pink
                deriving (Show, Read, Ord, Eq, Enum, Bounded)
-}

emptyColor = HSBK Nothing Nothing Nothing Nothing

isEmptyColor (HSBK Nothing Nothing Nothing Nothing) = True
isEmptyColor _ = False

isCompleteColor (HSBK (Just _ ) (Just _ ) (Just _ ) (Just _ )) = True
isCompleteColor _ = False

{-
customColor :: ColorArg -> MaybeColor
customColor (CNamed _ ) = HSBK Nothing Nothing Nothing Nothing
customColor (CCustom x) = x
-}

data Product =
  Product
  { pVendor :: !Word32
  , pProduct :: !Word32
  , pLongName :: Text
  , pShortName :: Text
  } deriving (Show, Eq, Ord)

products :: [Product]
products =
  [ Product 1 1 "LIFX Original 1000" "O1000"
  , Product 1 2 "LIFX Color 650"     "C650"
  , Product 1 3 "LIFX White 800"     "W800"
  ]

productFromId :: Word32 -> Word32 -> Maybe Product
productFromId v p = find f products
  where f (Product v' p' _ _) = v == v' && p == p'

productFromLongName :: Text -> Maybe Product
productFromLongName ln = find f products
  where f (Product _ _ ln' _) = ln == ln'

productFromShortName :: Text -> Maybe Product
productFromShortName sn = find f products
  where f (Product _ _ _ sn') = sn == sn'

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

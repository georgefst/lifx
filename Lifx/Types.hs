{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Lifx.Types where

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import Data.Hashable
import Data.List (find)
import Data.Monoid (Monoid(..))
import Data.Text (Text(..))
import qualified Data.Text.Encoding as TE
import Data.Word
import Debug.Trace

data HSBK a =
  HSBK
  { hue :: a
  , saturation :: a
  , brightness :: a
  , kelvin :: a
  } deriving (Show, Eq, Ord)


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


newtype DeviceId   = DeviceId B.ByteString   deriving (Eq, Ord, Hashable)
newtype GroupId    = GroupId B.ByteString    deriving (Eq, Ord, Hashable)
newtype LocationId = LocationId B.ByteString deriving (Eq, Ord, Hashable)

class LifxId t where
  toByteString :: t -> B.ByteString
  fromByteString :: B.ByteString -> t
  toText :: t -> Text
  fromText :: Text -> t

checkLength :: String -> Int -> B.ByteString -> B.ByteString
checkLength tname len bs
  | bslen == len = bs
  | otherwise =
      error ("when constructing " ++ tname
             ++ " from ByteString, expected "
             ++ show len ++ " bytes, but got " ++ show bslen)
  where bslen = B.length bs

idToText :: B.ByteString -> Text
idToText bs = TE.decodeUtf8 $ B16.encode bs

textToId :: String -> Int -> Text -> B.ByteString
textToId tname len txt
  | extralen /= 0 =
      error ("Got crud " ++ show extra ++ " after " ++ tname)
  | bslen == len = bs
  | otherwise =
      error ("when constructing " ++ tname
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
  fromByteString bs = DeviceId $ checkLength "DeviceId" deviceIdLen bs
  toText (DeviceId bs) = idToText bs
  fromText txt = DeviceId $ textToId "DeviceId" deviceIdLen txt

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
  fromByteString bs = GroupId $ checkLength "GroupId" groupIdLen bs
  toText (GroupId bs) = idToText bs
  fromText txt = GroupId $ textToId "GroupId" groupIdLen txt

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
  fromByteString bs = LocationId $ checkLength "LocationId" locationIdLen bs
  toText (LocationId bs) = idToText bs
  fromText txt = LocationId $ textToId "LocationId" locationIdLen txt

instance Show LocationId where
  showsPrec _ (LocationId bs) pre = implShow bs pre

instance Read LocationId where
  readsPrec _ s = implRead LocationId locationIdLen s

instance Binary LocationId where
  put (LocationId bs) = putByteString bs
  get = LocationId <$> getByteString locationIdLen


data Selector = SelAll
              | SelLabel Text
              | SelDevId DeviceId
              | SelGroup Text
              | SelGroupId GroupId
              | SelLocation Text
              | SelLocationId LocationId
                deriving (Show, Eq, Ord)

data ColorArg = CNamed  NamedColor
              | CCustom MaybeColor
                deriving (Show, Eq, Ord)

data NamedColor = White | Red | Orange | Yellow
                | Cyan | Green | Blue | Purple | Pink
                deriving (Show, Read, Ord, Eq, Enum, Bounded)

emptyColor = CCustom $ HSBK Nothing Nothing Nothing Nothing

isEmptyColor (CCustom (HSBK Nothing Nothing Nothing Nothing)) = True
isEmptyColor _ = False

isCompleteColor (CNamed _) = True
isCompleteColor (CCustom (HSBK (Just _ ) (Just _ ) (Just _ ) (Just _ ))) = True
isCompleteColor _ = False

customColor :: ColorArg -> MaybeColor
customColor (CNamed _ ) = HSBK Nothing Nothing Nothing Nothing
customColor (CCustom x) = x

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

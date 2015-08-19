{-# LANGUAGE OverloadedStrings #-}

module Lifx.Program.Types where

import Data.List (find)
import Data.Monoid (Monoid(..))
import Data.Scientific
import Data.Text (Text(..))
import Data.Word

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

type LiFrac = Scientific
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
combineMaybe _ x@(Just _) = x

newtype DeviceId = DeviceId Text deriving (Show, Eq, Ord)
newtype GroupId = GroupId Text deriving (Show, Eq, Ord)
newtype LocId = LocId Text deriving (Show, Eq, Ord)

data Selector = SelAll
              | SelLabel Text
              | SelDevId DeviceId
              | SelGroup Text
              | SelGroupId GroupId
              | SelLocation Text
              | SelLocationId LocId
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

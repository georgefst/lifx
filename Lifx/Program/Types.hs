module Lifx.Program.Types where

import Data.Monoid
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

type HSBK16 = HSBK Word16
type Color = HSBK LiFrac
type MaybeColor = HSBK (Maybe LiFrac)

newtype DeviceId = DeviceId Text
newtype GroupId = GroupId Text
newtype LocId = LocId Text

data Selector = SelAll
              | SelLabel Text
              | SelDevId DeviceId
              | SelGroup Text
              | SelGroupId GroupId
              | SelLocation Text
              | SelLocationId LocId

data ColorArg = CNamed  NamedColor
              | CCustom MaybeColor

data NamedColor = White | Red | Orange | Yellow
                | Cyan | Green | Blue | Purple | Pink
                deriving (Show, Read, Ord, Eq, Enum)

emptyColor = CCustom $ HSBK Nothing Nothing Nothing Nothing

isEmptyColor (CCustom (HSBK Nothing Nothing Nothing Nothing)) = True
isEmptyColor _ = False

isCompleteColor (CNamed _) = True
isCompleteColor (CCustom (HSBK (Just _ ) (Just _ ) (Just _ ) (Just _ ))) = True
isCompleteColor _ = False

customColor :: ColorArg -> MaybeColor
customColor (CNamed _ ) = HSBK Nothing Nothing Nothing Nothing
customColor (CCustom x) = x


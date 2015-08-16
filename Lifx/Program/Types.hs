module Lifx.Program.Types where

import Data.Monoid
import Data.Scientific
import Data.Word

data HSBK a =
  HSBK
  { hue :: a
  , saturation :: a
  , brightness :: a
  , kelvin :: a
  } deriving (Show, Eq, Ord)

{-
instance Functor HSBK where
  fmap f x = HSBK { hue = fmap f $ hue x
                  , saturation = fmap f $ saturation x
                  , brightness = fmap f $ brightness x
                  , kelvin = f $ fmap kelvin x
                  }
-}

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

type HSBK16 = HSBK Word16
type Color = HSBK Scientific
type MaybeColor = HSBK (Maybe Scientific)

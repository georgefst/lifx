{-# LANGUAGE OverloadedStrings, MultiWayIf #-}

module Lifx.ColorParser (parseColor) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import qualified Data.Text as T

import Lifx.Types
import Lifx.Util

-- | Parses a string as a color, returning either an error message or
-- a 'MaybeColor' which contains the HSBK color (or partial HSBK color)
-- specified by the string.  Accepts colors in
-- <http://api.developer.lifx.com/docs/colors the same format> as the
-- LIFX Cloud API.
parseColor :: T.Text -> Maybe MaybeColor
parseColor = parseAllMaybe colorString

colorString :: Parser MaybeColor
colorString = colorName <|> colorHSBK <|> colorHexRGB <|> colorDecRGB

colorName =
  choice
  [ asciiCI "white"  >> return white
  , asciiCI "red"    >> return red
  , asciiCI "orange" >> return orange
  , asciiCI "yellow" >> return yellow
  , asciiCI "green"  >> return green
  , asciiCI "cyan"   >> return cyan
  , asciiCI "blue"   >> return blue
  , asciiCI "purple" >> return purple
  , asciiCI "pink"   >> return pink
  ]

colorHSBK = do
  colors <- many1' $ do
    skipSpace
    f <- choice [ asciiCI "hue:"        >> return (\x c -> c { hue = x })
                , asciiCI "saturation:" >> return (\x c -> c { saturation = x })
                , asciiCI "brightness:" >> return (\x c -> c { brightness = x })
                , asciiCI "kelvin:"     >> return (\x c -> c { kelvin = x })
                ]
    n <- double
    return (f $ Just n)
  return $ foldl (\c f -> f c) emptyColor colors

hexCombine x y = x * 16 + y

hexDigit = digitToInt <$> satisfy isHexDigit

colorHexRGB = do
  char '#'
  r <- hexCombine <$> hexDigit <*> hexDigit
  g <- hexCombine <$> hexDigit <*> hexDigit
  b <- hexCombine <$> hexDigit <*> hexDigit
  return $ rgbToHsbk r g b

colorDecRGB = do
  asciiCI "rgb:"
  r <- decimal
  char ','
  g <- decimal
  char ','
  b <- decimal
  return $ rgbToHsbk r g b

rgbToHsbk :: Int -> Int -> Int -> MaybeColor
rgbToHsbk r' g' b' =
  let
    rgb = map ((/ 255) . fromIntegral) [r', g', b']
    [r, g, b] = rgb
    -- https://en.wikipedia.org/wiki/HSL_and_HSV#Hue_and_chroma
    mx = maximum rgb
    mn = minimum rgb
    c = mx - mn
    wrap x = if x < 0 then x + 6 else x
    h' = if | c == 0 -> 0
            | mx == r   -> wrap ((g - b) / c)
            | mx == g   -> 2 +  ((b - r) / c)
            | otherwise -> 4 +  ((r - g) / c)
    h = 60 * h'
    -- https://en.wikipedia.org/wiki/HSL_and_HSV#Lightness
    v = mx
    -- https://en.wikipedia.org/wiki/HSL_and_HSV#Saturation
    s = if c == 0 then 0 else c / v
  in HSBK (Just h) (Just s) (Just v) Nothing

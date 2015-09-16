{-# LANGUAGE OverloadedStrings #-}

module Lifx.ColorParser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char

import Lifx.Types

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
rgbToHsbk = undefined

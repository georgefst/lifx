{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Monad
import Data.Maybe
import Test.Tasty.HUnit

import Lifx

fromRight :: Either String a -> a
fromRight = either error id

fromRight' :: Show a => Either a b -> b
fromRight' = either (error . show) id

justColor :: Color -> PartialColor
justColor = fmap Just

definitelyColor :: PartialColor -> Color
definitelyColor = fmap fromJust

fst3 :: (a, b, c) -> a
fst3 (x, _, _ ) = x

assertCloseEnough :: (Num a, Ord a, Show a)
                     => a
                     -> String
                     -> a
                     -> a
                     -> IO ()
assertCloseEnough fudge msg expected actual =
  unless (abs (expected - actual) < fudge) $
    assertFailure (msg ++ ": " ++ show expected ++ " and " ++
                   show actual ++ " not within " ++ show fudge)

assertCloseEnough360 :: (Num a, Ord a, Show a)
                        => a
                        -> String
                        -> a
                        -> a
                        -> IO ()
assertCloseEnough360 fudge msg expected actual =
  unless (abs (expected - actual) < fudge || abs (expected + 360 - actual) < fudge) $
    assertFailure (msg ++ ": " ++ show expected ++ " and " ++
                   show actual ++ " not within " ++ show fudge)

assertColorEqual :: String -> Color -> Color -> IO ()
assertColorEqual msg expected actual = do
  -- https://community.lifx.com/t/some-weird-observations-when-writing-automated-tests/1080
  assertCloseEnough360 (360 / 500) (msg ++ ": hue") (hue expected) (hue actual)
  assertCloseEnough (1 / 500) (msg ++ ": saturation") (saturation expected) (saturation actual)
  assertCloseEnough (1 / 500) (msg ++ ": brightness") (brightness expected) (brightness actual)
  assertCloseEnough 4 (msg ++ ": kelvin") (kelvin expected) (kelvin actual)

colors :: [PartialColor]
colors = [red, orange, yellow, green, cyan, blue, purple, pink]

completeColors :: [Color]
completeColors = map makeComplete colors

makeComplete :: PartialColor -> Color
makeComplete c = definitelyColor $ combineColors (justColor defaultColor) c

hueColor :: ColorChannel -> PartialColor
hueColor h = HSBK (Just h) (Just 1) Nothing Nothing

-- LIFX cloud seems to not change the hue if the saturation is 0.
-- So, set saturation to 0.1 in order to reset all four components.
-- https://community.lifx.com/t/some-weird-observations-when-writing-automated-tests/1080
defaultColor :: Color
defaultColor = HSBK 0 0.1 0.5 5000

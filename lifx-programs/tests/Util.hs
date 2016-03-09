{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Arrow
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Lifx

fromRight :: Either String a -> a
fromRight = either error id

fromRight' :: Show a => Either a b -> b
fromRight' = either (error . show) id

justColor :: Color -> MaybeColor
justColor = fmap Just

definitelyColor :: MaybeColor -> Color
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
  if abs (expected - actual) < fudge
  then return ()
  else assertFailure (msg ++ ": " ++ show expected ++ " and " ++
                      show actual ++ " not within " ++ show fudge)

assertCloseEnough360 :: (Num a, Ord a, Show a)
                        => a
                        -> String
                        -> a
                        -> a
                        -> IO ()
assertCloseEnough360 fudge msg expected actual =
  if abs (expected - actual) < fudge || abs (expected + 360 - actual) < fudge
  then return ()
  else assertFailure (msg ++ ": " ++ show expected ++ " and " ++
                      show actual ++ " not within " ++ show fudge)

assertColorEqual :: String -> Color -> Color -> IO ()
assertColorEqual msg expected actual = do
  -- https://community.lifx.com/t/some-weird-observations-when-writing-automated-tests/1080
  assertCloseEnough360 (360 / 500) (msg ++ ": hue") (hue expected) (hue actual)
  assertCloseEnough (1 / 500) (msg ++ ": saturation") (saturation expected) (saturation actual)
  assertCloseEnough (1 / 500) (msg ++ ": brightness") (brightness expected) (brightness actual)
  assertCloseEnough 3 (msg ++ ": kelvin") (kelvin expected) (kelvin actual)

colors :: [MaybeColor]
colors = [red, orange, yellow, green, cyan, blue, purple, pink]

completeColors :: [Color]
completeColors = map makeComplete colors

makeComplete :: MaybeColor -> Color
makeComplete c = definitelyColor $ combineColors (justColor defaultColor) c

hueColor :: LiFrac -> MaybeColor
hueColor h = HSBK (Just h) (Just 1) Nothing Nothing

-- LIFX cloud seems to not change the hue if the saturation is 0.
-- So, set saturation to 0.1 in order to reset all four components.
-- https://community.lifx.com/t/some-weird-observations-when-writing-automated-tests/1080
defaultColor :: Color
defaultColor = HSBK 0 0.1 0.5 5000

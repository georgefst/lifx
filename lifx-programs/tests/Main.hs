{-# LANGUAGE OverloadedStrings #-}

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

import Util
import PureTests
import HardwareTests

main =
  defaultMain $ testGroup "All Tests"
    [ pureTests
    , hardwareTests
    ]

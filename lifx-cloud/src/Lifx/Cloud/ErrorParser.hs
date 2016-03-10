{-# LANGUAGE OverloadedStrings #-}

module Lifx.Cloud.ErrorParser (parseError) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import qualified Data.Text as T

import Lifx
import Lifx.Internal

parseError :: T.Text -> Maybe LifxException
parseError = parseAllMaybe errorMessage

errorMessage :: Parser LifxException
errorMessage =
  asciiCI "Could not find " >> SelectorNotFound <$> choice
  [ asciiCI "light with label: " >> selParse SelLabel
  , asciiCI "light with id: "    >> selParse SelDevId
  , asciiCI "group: "            >> selParse SelGroup
  , asciiCI "group_id: "         >> selParse SelGroupId
  , asciiCI "location: "         >> selParse SelLocation
  , asciiCI "location_id: "      >> selParse SelLocationId
  , asciiCI "scene_id: "         >> selParse SelSceneId
  ]

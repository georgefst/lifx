{-# LANGUAGE OverloadedStrings #-}

module Lifx.Cloud.ErrorParser (parseError) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import qualified Data.Text as T

import Lifx
import Lifx.Internal

parseError :: T.Text -> Maybe LifxException
parseError = parseAllMaybe anyError

anyError :: Parser LifxException
anyError = selectorError <|> (BadParam <$> paramError)

selectorError :: Parser LifxException
selectorError =
  asciiCI "Could not find " >> SelectorNotFound <$> choice
  [ asciiCI "light with label: " >> selParse SelLabel
  , asciiCI "light with id: "    >> selParse SelDevId
  , asciiCI "group: "            >> selParse SelGroup
  , asciiCI "group_id: "         >> selParse SelGroupId
  , asciiCI "location: "         >> selParse SelLocation
  , asciiCI "location_id: "      >> selParse SelLocationId
  , asciiCI "scene_id: "         >> selParse SelSceneId
  ]

paramError :: Parser ParamError
paramError = tooMany <|> nameFirst

anythingBut :: Char -> Parser T.Text
anythingBut c = takeWhile1 (/= c)

-- "Too many selectors; max is 25 but got 26"
tooMany :: Parser ParamError
tooMany = do
  asciiCI "Too many "
  name <- anythingBut ';'
  asciiCI "; max is "
  mx <- decimal
  asciiCI " but got "
  takeWhile1 isDigit
  -- assume minimum is 0, since it is not given
  return $ InvalidEntries name 0 mx

nameFirst :: Parser ParamError
nameFirst = do
  name <- anythingBut ' '
  choice [ -- "states[duration] does not have a valid value"
           do asciiCI " does not have a valid value"
              return $ InvalidParam name
         , -- "hue is outside of range: 0-360"
           do asciiCI " is outside of range: "
              mn <- double
              char '-'
              mx <- double
              return $ InvalidRange name mn mx
         , -- "states must have between 2 and 10 entries"
           do asciiCI " must have between "
              mn <- decimal
              asciiCI " and "
              mx <- decimal
              asciiCI " entries"
              return $ InvalidEntries name mn mx
         ]

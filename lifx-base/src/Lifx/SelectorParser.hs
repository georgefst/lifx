{-# LANGUAGE OverloadedStrings #-}

module Lifx.SelectorParser (parseSelector) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import qualified Data.Text as T

import Lifx.Types

parseSelector :: T.Text -> Either String Selector
parseSelector = parseOnly (selectorString <* endOfInput)

selectorString :: Parser Selector
selectorString = selAll <|> selLabel <|> selDevId <|>
                 selGroup <|> selGroupId <|> selLocation <|> selLocationId

selParse :: LifxId a => (a -> Selector) -> Parser Selector
selParse cnstr = do
  txt <- takeText
  case fromText txt of
   Left msg -> fail msg
   Right x -> return $ cnstr x

selAll        = asciiCI "all"          >> return   SelAll
selLabel      = asciiCI "label:"       >> selParse SelLabel
selDevId      = asciiCI "id:"          >> selParse SelDevId
selGroup      = asciiCI "group:"       >> selParse SelGroup
selGroupId    = asciiCI "group_id:"    >> selParse SelGroupId
selLocation   = asciiCI "location:"    >> selParse SelLocation
selLocationId = asciiCI "location_id:" >> selParse SelLocationId

{-# LANGUAGE OverloadedStrings #-}

module Lifx.SelectorParser (parseSelector, parseSelectors) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import qualified Data.Text as T

import Lifx.Types

parseSelector :: T.Text -> Either String Selector
parseSelector = parseOnly (selectorString <* endOfInput)

parseSelectors :: T.Text -> Either String [Selector]
parseSelectors = parseOnly (selectorList <* endOfInput)

selectorString :: Parser Selector
selectorString = selAll <|> selLabel <|> selDevId <|>
                 selGroup <|> selGroupId <|> selLocation <|> selLocationId

selectorList :: Parser [Selector]
selectorList = selectorString `sepBy1` (char ',')

selParse :: LifxId a => (a -> Selector) -> Parser Selector
selParse cnstr = do
  txt <- takeWhile1 (/= ',')
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

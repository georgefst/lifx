{-# LANGUAGE OverloadedStrings #-}

module Lifx.ProductShortName (productShortName) where

import Control.Applicative
import Data.Attoparsec.Text hiding (takeWhile)
import qualified Data.Attoparsec.Text as A (takeWhile)
import Data.Char
import Data.Monoid
import qualified Data.Text as T

import Lifx.Types

productShortName :: T.Text -> T.Text
productShortName t = f (parseOnly pname t)
  where f (Left _)  = "???"
        f (Right x) = x

pname :: Parser T.Text
pname = do
  skipNonAlnum
  t1 <- option "" aLetter
  skipNonDigit
  t2 <- someDigits
  skipNonAlpha
  t3 <- option "" aLetter
  return $ t1 <> t2 <> t3

skipNonAlnum = skipWhile (not . isAlphaNum)
skipNonDigit = skipWhile (not . isDigit)
skipNonAlpha = skipWhile (not . isAlpha)

aLetter = T.singleton <$> letter
someDigits = A.takeWhile isDigit

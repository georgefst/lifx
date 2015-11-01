{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Lifx.ProductTable (products, productFromId) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString as B
import Data.Either
import Data.FileEmbed
import Data.List
import qualified Data.Text as T
import Data.Word

import Lifx.ProductShortName
import Lifx.Types

products :: [Product]
products = fromRight $ eitherDecodeStrict' $(embedFile "products/products.json")

productFromId :: Word32 -> Word32 -> Maybe Product
productFromId v p = find f products
  where f (Product v' p' _ _ _ ) = v == v' && p == p'

fromRight = either error id

colorYes = Capabilities { cHasColor = True,  cHasVariableColorTemp = True }
colorNo  = Capabilities { cHasColor = False, cHasVariableColorTemp = True }

instance FromJSON Product where
  parseJSON (Object v) = do
    longName <-          v .: "Name"
    vendor   <- read <$> v .: "Vendor ID"
    prod     <- read <$> v .: "Product ID"
    hasColor <-          v .: "Color"
    caps <- case (hasColor :: T.Text) of
             "Yes" -> return colorYes
             "No"  -> return colorNo
             _     -> fail $ "expected Yes or No, but got " ++ show hasColor
    return $ Product vendor prod longName (productShortName longName) caps

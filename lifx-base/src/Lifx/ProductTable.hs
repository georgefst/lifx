{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Lifx.ProductTable (productFromId) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString as B
import Data.Char
import Data.Either
import Data.FileEmbed
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word

import Lifx.Types

data Vendor =
  Vendor
  { vVid  :: !Word32
  , vName :: T.Text
  , vProducts :: [ProductForVendor]
  } deriving (Show, Read, Eq, Ord)

data ProductForVendor =
  ProductForVendor
  { vpPid      :: !Word32
  , vpName     :: T.Text
  , vpFeatures :: Capabilities
  } deriving (Show, Read, Eq, Ord)

data VidPid = VidPid !Word32 !Word32 deriving (Show, Read, Eq, Ord)

instance FromJSON Vendor where
  parseJSON (Object v) = do
    vid   <- v .: "vid"
    name  <- v .: "name"
    prods <- v .: "products"
    return $ Vendor vid name prods

instance FromJSON ProductForVendor where
  parseJSON (Object v) = do
    pid      <- v .: "pid"
    name     <- v .: "name"
    features <- v .: "features"
    color    <- features .: "color"
    return $ ProductForVendor pid name $ if color then colorYes else colorNo

vendors :: [Vendor]
vendors = fromRight $ eitherDecodeStrict' $(embedFile "products/products.json")

products :: M.Map VidPid Product
products = M.fromList $ concatMap productsForVendor vendors

productsForVendor :: Vendor -> [(VidPid, Product)]
productsForVendor v = map pr2pr (vProducts v)
  where pr2pr p = (VidPid (vVid v) (vpPid p),
                   Product { pCompanyName  = vName v
                           , pProductName  = vpName p
                           , pIdentifier   = mkIdentifier (vName v) (vpName p)
                           , pCapabilities = vpFeatures p
                           })

mkIdentifier :: T.Text -> T.Text -> T.Text
mkIdentifier v p = T.toLower $ T.map underscorify $ T.concat [v, " ", p]
  where underscorify c = if isAlphaNum c then c else '_'

productFromId :: Word32 -> Word32 -> Maybe Product
productFromId v p = M.lookup (VidPid v p) products

fromRight = either error id

colorYes = Capabilities { cHasColor = True,  cHasVariableColorTemp = True }
colorNo  = Capabilities { cHasColor = False, cHasVariableColorTemp = True }

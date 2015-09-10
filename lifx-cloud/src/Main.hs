{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.List
import Data.Maybe
import System.IO

cstat _ _ _ = Nothing

main = do
  lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
  let lifxToken = B8.pack $ takeWhile (not . isSpace) lifxTokenStr
  mgr <- newManager tlsManagerSettings
  req <- parseUrl "https://api.lifx.com/v1beta1/lights/all"
  let req' = applyBasicAuth lifxToken "" $ req { checkStatus = cstat }
  resp <- httpLbs req' mgr
  let lbs = responseBody resp
      val = (fromJust $ decode lbs) :: Value
      pretty = encodePretty val
  L.putStr pretty
  putStrLn ""

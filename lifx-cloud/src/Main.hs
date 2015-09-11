{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Version
import System.IO

import Paths_lifx_cloud

pkg_name :: B.ByteString
pkg_name = "lifx-cloud"

cstat _ _ _ = Nothing

encPretty = encodePretty' (defConfig { confCompare = cmp })
  where cmp = keyOrder ko <> compare
        ko = [ "id", "name", "selector"
             , "uuid", "label", "connected", "power"
             , "color"
             , "hue", "saturation", "brightness", "kelvin"
             , "group", "location"
             , "last_seen", "seconds_since_seen"
             , "product_name", "capabilities"
             , "duration", "status"
             , "updated_at", "created_at", "account", "devices"
             , "has_color", "has_variable_color_temp"
             , "operation", "results"
             ]

userAgent = (hUserAgent, B.concat ua)
  where ua = [ pkg_name, "/", B8.pack (showVersion version)
             , " (+https://community.lifx.com/t/what-are-you-building/26/39)"
             ]

addHeaders :: B.ByteString -> Request -> Request
addHeaders lifxToken req =
  applyBasicAuth lifxToken "" $ req
    { checkStatus = cstat
    , requestHeaders = userAgent : requestHeaders req
    }

main = do
  lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
  let lifxToken = B8.pack $ takeWhile (not . isSpace) lifxTokenStr
  mgr <- newManager tlsManagerSettings
  req <- parseUrl "https://api.lifx.com/v1beta1/lights/all"
  let req' = addHeaders lifxToken req
  resp <- httpLbs req' mgr
  let lbs = responseBody resp
      val = (fromJust $ decode lbs) :: Value
      pretty = encPretty val
  L.putStr pretty
  putStrLn ""

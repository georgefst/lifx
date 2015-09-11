{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Control.Concurrent
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.Char
import qualified Data.HashMap.Strict as H
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import Data.Vector hiding (takeWhile, mapM_, (++))
import Data.Version
import System.IO

import Paths_lifx_cloud

pkg_name :: B.ByteString
pkg_name = "lifx-cloud"

cstat _ _ _ = Nothing

{-
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
-}

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

addConType :: B.ByteString -> Request -> Request
addConType ct req = req
  { requestHeaders = contentType : requestHeaders req }
  where contentType = (hContentType, ct)

jsonPut :: Request -> Request
jsonPut req = addConType "application/json" req'
  where req' = req { method = methodPut }

statesFromColor :: T.Text -> Value
statesFromColor c =
  object [ ("states",
            Array $ fromList
            [ object [ ("selector", String "id:d073d50225cd")
                     , ("color", String c) ] ] ) ]

setColor :: Manager -> B.ByteString -> T.Text -> IO ()
setColor mgr lifxToken c = do
  req <- parseUrl "https://api.lifx.com/v1.0-beta1/lights/states"
  let body = RequestBodyLBS $ encode $ statesFromColor c
      req' = (addHeaders lifxToken req) { requestBody = body }
  httpLbs (jsonPut req') mgr
  return ()
{-
  let lbs = responseBody resp
      val = (fromJust $ decode lbs) :: Value
      pretty = encPretty val
  L.putStr pretty
  putStrLn ""
-}

getColor :: Manager -> B.ByteString -> T.Text -> IO ()
getColor mgr lifxToken c = do
  req <- parseUrl "https://api.lifx.com/v1beta1/lights/id:d073d50225cd"
  let req' = addHeaders lifxToken req
  resp <- httpLbs req' mgr
  let lbs = responseBody resp
      (Object val) = (fromJust $ decode lbs) :: Value
      (Just (Object color)) = H.lookup "color" val
      (Just (Number h)) = H.lookup "hue" color
      (Just (Number s)) = H.lookup "saturation" color
  putStrLn $ T.unpack c ++ " = HSBK (Just " ++ show (round h) ++
    ") (Just " ++ show (round s) ++ ") Nothing Nothing"

doColor :: Manager -> B.ByteString -> T.Text -> IO ()
doColor mgr lifxToken c = do
  setColor mgr lifxToken c
  threadDelay 2000000
  getColor mgr lifxToken c

main = do
  lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
  let lifxToken = B8.pack $ takeWhile (not . isSpace) lifxTokenStr
  mgr <- newManager tlsManagerSettings
{-
  req <- parseUrl "https://api.lifx.com/v1beta1/lights/all"
  let req' = addHeaders lifxToken req
  resp <- httpLbs req' mgr
  let lbs = responseBody resp
      val = (fromJust $ decode lbs) :: Value
      pretty = encPretty val
  L.putStr pretty
  putStrLn ""
-}
  mapM_ (doColor mgr lifxToken)
    ["white", "red", "orange", "yellow", "cyan",
     "green", "blue", "purple", "pink"]

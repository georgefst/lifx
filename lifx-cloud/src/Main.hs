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
import qualified Data.Text.Encoding as TE
import Data.Vector hiding (takeWhile, mapM_, (++))
import Data.Version
import System.IO

import Lifx.Types hiding (listLights)

import Paths_lifx_cloud

pkg_name :: B.ByteString
pkg_name = "lifx-cloud"

data CloudConnection =
  CloudConnection
  { ccManager :: Manager
  , ccToken :: AuthToken
  , ccRoot :: T.Text
  }

endpoint :: CloudConnection -> T.Text -> IO Request
endpoint cc ep = do
  let url = T.unpack $ ccRoot cc <> ep
  req <- parseUrl url
  return $ addHeaders (TE.encodeUtf8 $ toText $ ccToken cc) req

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

appJson = "application/json"

addHeaders :: B.ByteString -> Request -> Request
addHeaders lifxToken req =
  applyBasicAuth lifxToken "" $ req
    { checkStatus = cstat
    , requestHeaders = userAgent : accept : requestHeaders req
    }
  where accept = (hAccept, appJson)

addConType :: B.ByteString -> Request -> Request
addConType ct req = req
  { requestHeaders = contentType : requestHeaders req }
  where contentType = (hContentType, ct)

jsonPut :: Request -> L.ByteString -> Request
jsonPut req body = addConType appJson req'
  where req' = req { method = methodPut, requestBody = RequestBodyLBS body }

{-
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
  let lbs = responseBody resp
      val = (fromJust $ decode lbs) :: Value
      pretty = encPretty val
  L.putStr pretty
  putStrLn ""

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
-}

listLights :: CloudConnection -> T.Text -> IO L.ByteString
listLights cc sel = do
  req <- endpoint cc ("lights/" <> sel)
  resp <- httpLbs req (ccManager cc)
  return $ responseBody resp

setStates :: CloudConnection -> L.ByteString -> IO L.ByteString
setStates cc states = do
  req <- endpoint cc "lights/states"
  let req' = jsonPut req states
  resp <- httpLbs req' (ccManager cc)
  return $ responseBody resp

doEffect :: CloudConnection
            -> T.Text
            -> T.Text
            -> [(B.ByteString, B.ByteString)]
            -> IO L.ByteString
doEffect cc sel eff params = do
  req <- endpoint cc ("lights/" <> sel <> "/effects/" <> eff)
  let req' = urlEncodedBody params req
  resp <- httpLbs req' (ccManager cc)
  return $ responseBody resp

listScenes :: CloudConnection -> IO L.ByteString
listScenes cc = do
  req <- endpoint cc "scenes"
  resp <- httpLbs req (ccManager cc)
  return $ responseBody resp

activateScene :: CloudConnection
                 -> T.Text
                 -> [(B.ByteString, B.ByteString)]
                 -> IO L.ByteString
activateScene cc scene params = do
  req <- endpoint cc ("scenes/scene_id:" <> scene <> "/activate")
  let req' = (urlEncodedBody params req) { method = methodPut }
  resp <- httpLbs req' (ccManager cc)
  return $ responseBody resp

fromRight = either error id

main = do
  lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
  let lifxToken = fromRight $ fromText $ T.pack $ takeWhile (not . isSpace) lifxTokenStr
  mgr <- newManager tlsManagerSettings
  let cc = CloudConnection mgr lifxToken "https://api.lifx.com/v1.0-beta1/"
  -- lbs <- doEffect cc "id:d073d50225cd" "pulse" [ ("color", "red") , ("cycles", "5") ]
  -- lbs <- listScenes cc
  -- lbs <- activateScene cc "ffae25ad-f74d-458f-af37-73b958921b18" [("duration", "5")]
  lbs <- listLights cc "all"
  let lites = (eitherDecode lbs) :: Either String [LightInfo]
  print lites
  {-
  let val = (fromJust $ decode lbs) :: Value
      pretty = encPretty val
  L.putStr pretty
  putStrLn ""
  -}
{-
  mapM_ (doColor mgr lifxToken)
    ["white", "red", "orange", "yellow", "cyan",
     "green", "blue", "purple", "pink"]
-}

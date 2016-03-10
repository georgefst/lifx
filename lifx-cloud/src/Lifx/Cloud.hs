{-# LANGUAGE OverloadedStrings #-}

module Lifx.Cloud
       ( CloudSettings(..)
       , CloudConnection
       , defaultCloudSettings
       , openCloudConnection
       ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Parser)
-- import Data.Aeson.Encode.Pretty
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
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import Data.Text.Format hiding (print)
import Data.Text.Format.Params
import Data.Vector hiding (takeWhile, mapM_, (++), map, forM)
import Data.Version
import System.IO

import Lifx
import Lifx.Internal

import Lifx.Cloud.Json

import Paths_lifx_cloud

pkg_name :: B.ByteString
pkg_name = "lifx-cloud"

data CloudSettings =
  CloudSettings
  { csManager :: IO Manager
  , csToken :: AccessToken
  , csRoot :: T.Text
  , csLog :: T.Text -> IO ()
  }

defaultCloudSettings :: CloudSettings
defaultCloudSettings =
  CloudSettings
  { csManager = newManager tlsManagerSettings
  , csToken = error "You need to specify a valid API token for csToken."
  , csRoot = "https://api.lifx.com/v1/"
  , csLog = TIO.hPutStrLn stderr
  }

data CloudConnection =
  CloudConnection
  { ccManager :: !Manager
  , ccToken :: !AccessToken
  , ccRoot :: !T.Text
  , ccWarn :: T.Text -> IO ()
  }

wrapHttpException :: HttpException -> IO a
wrapHttpException e = throwIO $ CloudHttpError (T.pack $ show e) (toException e)

openCloudConnection :: CloudSettings -> IO CloudConnection
openCloudConnection cs = do
  mgr <- csManager cs `catch` wrapHttpException
  return $ CloudConnection { ccManager = mgr
                           , ccToken = csToken cs
                           , ccRoot = csRoot cs
                           , ccWarn = csLog cs
                           }

decodeUtf8Lenient :: B.ByteString -> T.Text
decodeUtf8Lenient = TE.decodeUtf8With TEE.lenientDecode

newtype CloudMessage = CloudMessage { unCloudMessage :: T.Text }

instance FromJSON CloudMessage where
  parseJSON (Object v) = CloudMessage <$> v .: "error"
  parseJSON _ = mzero

newtype ResultWrapper = ResultWrapper { unResultWrapper :: [Result] }

instance FromJSON ResultWrapper where
  parseJSON (Object v) = ResultWrapper <$> v .: "results"
  parseJSON _ = mzero

isJsonMimeType :: Response a -> Bool
isJsonMimeType resp =
  let hdrs = responseHeaders resp
      mayMime = hContentType `lookup` hdrs
  in case mayMime of
      Nothing -> False
      Just bs -> bs == appJson

extractMessage :: Response L.ByteString -> T.Text
extractMessage resp = orElseStatus jsonMessage
  where orElseStatus Nothing =
          fmt "{} {}"
          ( statusCode $ responseStatus resp
          , decodeUtf8Lenient $ statusMessage $ responseStatus resp )
        orElseStatus (Just txt) = txt
        jsonMessage = do
          unless (isJsonMimeType resp) Nothing
          msg <- decode' $ responseBody resp
          return $ unCloudMessage msg

newtype Warnings = Warnings [T.Text]

instance FromJSON Warnings where
  parseJSON (Object v) = do
    ws <- v .: "warnings"
    Warnings <$> forM ws warnToText
  parseJSON _ = mzero

warnToText :: Object -> Parser T.Text
warnToText w = do
  warning <- w .: "warning"
  unkp <- w .:? "unknown_params" :: Parser (Maybe Object)
  return $ case unkp of
            Nothing -> warning
            (Just p) -> warning <> ": " <> T.intercalate ", " (H.keys p)

logWarnings :: (T.Text -> IO ()) -> Maybe Warnings -> IO ()
logWarnings _ Nothing = return ()
logWarnings _ (Just (Warnings [])) = return ()
logWarnings log (Just (Warnings w)) =
  log $ "LIFX cloud warning: " <> (T.intercalate "; " w)

performRequest :: FromJSON a => CloudConnection -> Request -> IO a
performRequest cc req = performRequest' cc req `catch` wrapHttpException

performRequest' :: FromJSON a => CloudConnection -> Request -> IO a
performRequest' cc req = do
  resp <- httpLbs req (ccManager cc)
  let stat = responseStatus resp
      code = statusCode stat
  when (code < 200 || code > 299) $ throwIO $ CloudError $ extractMessage resp
  let body = responseBody resp
  -- decode the body as the return type
  case eitherDecode' body of
   Left msg -> throwIO $ CloudJsonError (T.pack msg) body
   Right x -> do
     -- decode the body a second time as type Warnings to get any warnings
     logWarnings (ccWarn cc) (decode' body)
     return x

newtype StatePair = StatePair ([Selector], StateTransition)

selectorsToTextThrow :: [Selector] -> T.Text
selectorsToTextThrow sels = f $ selectorsToText sels
  where f (Left e) = throw e
        f (Right t) = t

selectorsToTextThrowIO :: [Selector] -> IO T.Text
selectorsToTextThrowIO sels = f $ selectorsToText sels
  where f (Left e) = throwIO e
        f (Right t) = return t

instance ToJSON StatePair where
  toJSON (StatePair (sel, st)) =
    let ps = stateTransitionToPairs st
        ps' = ("selector" .= selectorsToTextThrow sel) : ps
    in object ps'

maybifyPair :: (B.ByteString, Maybe T.Text)
               -> Maybe (B.ByteString, B.ByteString)
maybifyPair (_, Nothing) = Nothing
maybifyPair (k, (Just v)) = Just (k, TE.encodeUtf8 v)

showDown :: Show a => a -> T.Text
showDown x = T.pack $ map toLower $ show x

instance Connection CloudConnection where
  listLights cc sel _ = do
    txt <- selectorsToTextThrowIO sel
    req <- endpoint cc ("lights/" <> txt)
    performRequest cc req

  setStates cc pairs = do
    req <- endpoint cc "lights/states"
    let states = encode $ object ["states" .= map StatePair pairs]
        req' = jsonPut req states
    performRequest cc req'

  togglePower cc sel dur = do
    txt <- selectorsToTextThrowIO sel
    req <- endpoint cc ("lights/" <> txt <> "/toggle")
    let durTxt = fmt "{}" (Only dur)
        params = [("duration", TE.encodeUtf8 durTxt)]
        req' = (urlEncodedBody params req)
    unResultWrapper <$> performRequest cc req'

  effect cc sel eff = do
    txt <- selectorsToTextThrowIO sel
    req <- endpoint cc ("lights/" <> txt <> "/effects/" <> showDown (eType eff))
    let params = mapMaybe maybifyPair
                 [ ("color",      colorToText (eColor eff))
                 , ("from_color", colorToText (eFromColor eff))
                 , ("period",     Just $ fmt "{}" (Only $ ePeriod eff))
                 , ("cycles",     Just $ fmt "{}" (Only $ eCycles eff))
                 , ("persist",    Just $ showDown $ ePersist eff)
                 , ("power_on",   Just $ showDown $ ePowerOn eff)
        -- https://community.lifx.com/t/peak-parameter-for-pulse-endpoint/1083
                 , ("peak", case eType eff of
                             Breathe -> Just $ fmt "{}" (Only $ ePeak eff)
                             _       -> Nothing)
                 ]
        req' = (urlEncodedBody params req)
    unResultWrapper <$> performRequest cc req'

  listScenes cc = do
    req <- endpoint cc "scenes"
    performRequest cc req

  activateScene cc scene dur = do
    req <- endpoint cc ("scenes/scene_id:" <> toText scene <> "/activate")
    let durTxt = fmt "{}" (Only dur)
        params = [("duration", TE.encodeUtf8 durTxt)]
        req' = (urlEncodedBody params req) { method = methodPut }
    unResultWrapper <$> performRequest cc req'

  cycleLights cc sel states = do
    txt <- selectorsToTextThrowIO sel
    req <- endpoint cc ("lights/" <> txt <> "/cycle")
    let states' = encode $ object ["states" .= states]
        req' = (jsonPut req states') { method = methodPost }
    unResultWrapper <$> performRequest cc req'

  closeConnection _ = return ()

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

{-|
Module      : System.Hardware.Lifx.Cloud
Description : Implementation of Connection for LIFX Cloud API
Copyright   : © Patrick Pelletier, 2016
License     : BSD3
Maintainer  : code@funwithsoftware.org
Stability   : experimental
Portability : GHC

This module implements a 'Connection' for controlling LIFX bulbs
via the LIFX Cloud API.
-}

{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.Lifx.Cloud
       ( CloudSettings(..)
       , UserAgentComponent(..)
       , CloudConnection
       , defaultCloudSettings
       , openCloudConnection
       , RateLimit (..)
       , getRateLimit
       ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson hiding (Result)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as I
import Data.Char
import qualified Data.HashMap.Strict as H
import Data.Hourglass
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import Data.Text.Format hiding (print)
import Data.Version
import Text.Read
import System.IO
import Time.System

import System.Hardware.Lifx
import System.Hardware.Lifx.Connection
import System.Hardware.Lifx.Internal

import System.Hardware.Lifx.Cloud.ErrorParser
import System.Hardware.Lifx.Cloud.Json
import System.Hardware.Lifx.Cloud.Preprocessor
import System.Hardware.Lifx.Cloud.Util

import Paths_lifx_highlevel

pkg_name :: T.Text
pkg_name = "lifx-highlevel"

pkg_url :: T.Text
pkg_url = "+https://github.com/ppelleti/hs-lifx"

-- | Parameters which can be passed to 'openCloudConnection'.
data CloudSettings =
  CloudSettings
  { -- | 'IO' action which returns a 'Manager' which is to be used for
    -- HTTP connections to the cloud.  Since the LIFX Cloud API uses @https@,
    -- the 'Manager' must support TLS.  Default action is to create a new
    -- 'Manager' which supports TLS.
    csManager :: IO Manager
    -- | 'IO' action which returns an 'AccessToken' for the cloud account
    -- to use.  The default action is to look for the access token in
    -- @~\/.config\/hs-lifx\/config.json@, and to throw 'NoAccessToken' if
    -- @token@ is not specified in the config file.
  , csToken :: IO AccessToken
    -- | The
    -- <https://en.wikipedia.org/wiki/User_agent#Use_in_HTTP User-Agent string>
    -- to use when talking to the LIFX Cloud.  The default contains versions
    -- for @lifx-cloud@ and several of the packages used by it.  It's
    -- recommended that you cons another 'UserAgentComponent' onto the
    -- beginning of the default, containing your package's name, version,
    -- and the URL of your project as the comment.
  , csUserAgent :: [UserAgentComponent]
    -- | Function to log a line of text.  This includes warnings from the
    -- cloud, and other information which might be helpful for troubleshooting.
    -- Default is 'TIO.hPutStrLn' 'stderr'.
  , csLog :: T.Text -> IO ()
    -- | Base URL for API endpoints.  Default is the correct value for the
    -- LIFX Cloud.  You should not change this.  The only reason to do so is
    -- if you want to mock the cloud for testing purposes.
  , csRoot :: T.Text
  }

defaultToken :: IO AccessToken
defaultToken = do
  cfg <- getConfig
  case cfgToken cfg of
   Nothing  -> do
     file <- configFile
     throwIO $ NoAccessToken file
   Just tok -> return tok

-- | Returns a 'CloudSettings' with default settings.
defaultCloudSettings :: CloudSettings
defaultCloudSettings =
  CloudSettings
  { csManager = newManager tlsManagerSettings
  , csToken = defaultToken
  , csUserAgent = defaultUserAgent
  , csLog = TIO.hPutStrLn stderr
  , csRoot = "https://api.lifx.com/v1/"
  }

-- | A structured representation of one element of a User-Agent string.
data UserAgentComponent =
  UserAgentComponent
  { -- | The name of the "product", such as a package name like "lifx-programs".
    uaProduct :: T.Text
    -- | The version of the product.  Cabal can
    -- <https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-the-package-version automatically tell you>
    -- the current version of your package.
  , uaVersion :: Version
    -- | Optionally, what the HTTP spec calls a "comment".  This is typically
    -- used to provide the URL of your project.
  , uaComment :: (Maybe T.Text)
  } deriving (Eq, Ord, Read, Show)

renderUserAgent :: [UserAgentComponent] -> B.ByteString
renderUserAgent xs = B.concat $ intercalate [" "] $ map rua xs
  where rua uac = [ (TE.encodeUtf8 $ uaProduct uac)
                  , "/"
                  , (B8.pack $ showVersion $ uaVersion uac)
                  ] ++ renderComment (uaComment uac)
        renderComment Nothing = []
        renderComment (Just com) = [" (", TE.encodeUtf8 $ escCom com, ")"]
        escCom x = T.pack $ concatMap escChar $ T.unpack x
        escChar '(' = "\\("
        escChar ')' = "\\)"
        escChar '\\' = "\\\\"
        escChar c = [c]

-- | Opaque type which implements 'Connection' and represents a connection
-- to a LIFX cloud account.  It's OK to use a @CloudConnection@ from
-- multiple threads at once.
data CloudConnection =
  CloudConnection
  { ccManager :: !Manager
  , ccToken :: !AccessToken
  , ccUserAgent :: !B.ByteString
  , ccRoot :: !T.Text
  , ccWarn :: T.Text -> IO ()
  , ccRateLimit :: TVar (Maybe RateLimit)
  }

-- | Information returned by the server about
-- <http://api.developer.lifx.com/docs/rate-limits rate limiting>.
-- Includes both the server time and the client time when the rate
-- limit was returned, to allow accounting for clock skew.
data RateLimit =
  RateLimit
  { -- | Total number of requests allowed per window
    rlLimit      :: !Int
    -- | Remaining number of requests in this window
  , rlRemaining  :: !Int
    -- | Time (according to server) that the next window begins
  , rlReset      :: DateTime
    -- | Time (according to server) that this information was returned
  , rlServerTime :: DateTime
    -- | Time (according to client) that this information was returned
  , rlClientTime :: DateTime
  } deriving (Eq, Ord, Show, Read)

-- | Get the rate limit information that was returned by the server
-- when the most recent request was made.  Returns 'Nothing' if this
-- is a brand new 'CloudConnection' which hasn't made a request yet.
getRateLimit :: CloudConnection -> IO (Maybe RateLimit)
getRateLimit cc = readTVarIO (ccRateLimit cc)

throwHttpException :: HttpException -> IO a
throwHttpException e = throwIO $ wrapHttpException e

wrapHttpException :: HttpException -> LifxException
wrapHttpException e = HttpError (T.pack $ show e) (toException e)

-- | Create a new 'CloudConnection', based on 'CloudSettings'.
openCloudConnection :: CloudSettings -> IO CloudConnection
openCloudConnection cs = do
  mgr <- csManager cs `catch` throwHttpException
  tok <- csToken cs
  rateLimit <- newTVarIO Nothing
  return $ CloudConnection { ccManager = mgr
                           , ccToken = tok
                           , ccUserAgent = renderUserAgent $ csUserAgent cs
                           , ccRoot = csRoot cs
                           , ccWarn = csLog cs
                           , ccRateLimit = rateLimit
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

-- Return an error message from a response.  Returns the error
-- message from the JSON body if possible, otherwise returns
-- the HTTP status.
extractMessage :: Response L.ByteString -> LifxException
extractMessage resp = orElseStatus jsonMessage
  where orElseStatus Nothing =
          HttpStatusError (statusCode st) (decodeUtf8Lenient $ statusMessage st)
        orElseStatus (Just txt) = mkExcep txt
        st = responseStatus resp
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

mkExcep :: T.Text -> LifxException
mkExcep msg =
  case parseError msg of
   (Just exc) -> exc
   Nothing -> RemoteError msg

performRequest :: FromJSON a => CloudConnection -> Request -> IO a
performRequest cc req = performRequest' cc req `catch` throwHttpException

performRequest' :: FromJSON a => CloudConnection -> Request -> IO a
performRequest' cc req = do
  getRateLimit cc >>= waitForRateLimit (ccWarn cc)
  resp <- httpLbs req (ccManager cc)
  updateRateLimit (ccRateLimit cc) (headersToRateLimit $ responseHeaders resp)
  let stat = responseStatus resp
      code = statusCode stat
  when (code < 200 || code > 299) $ throwIO $ extractMessage resp
  let body = responseBody resp
  -- decode the body as the return type
  case eitherDecode' body of
   Left msg -> throwIO $ JsonError (T.pack msg) body
   Right x -> do
     -- decode the body a second time as type Warnings to get any warnings
     logWarnings (ccWarn cc) (decode' body)
     return x

hLimit     = I.mk "X-RateLimit-Limit"
hRemaining = I.mk "X-RateLimit-Remaining"
hReset     = I.mk "X-RateLimit-Reset"

unBS :: B.ByteString -> Maybe String
unBS bs =
  case TE.decodeUtf8' bs of
   Left _ -> Nothing
   Right txt -> Just $ T.unpack txt

headersToRateLimit :: ResponseHeaders -> Maybe RateLimit
headersToRateLimit hdrs = do
  dateBS       <- hDate      `lookup` hdrs
  limitBS      <- hLimit     `lookup` hdrs
  remainingBS  <- hRemaining `lookup` hdrs
  resetBS      <- hReset     `lookup` hdrs
  dateStr      <- unBS dateBS
  limitStr     <- unBS limitBS
  remainingStr <- unBS remainingBS
  resetStr     <- unBS resetBS
  dateTime     <- timeParse MyRFC1123_DateAndTime $ drop 3 dateStr
  limitInt     <- readMaybe limitStr
  remainingInt <- readMaybe remainingStr
  resetInt     <- readMaybe resetStr
  return $ RateLimit { rlLimit = limitInt
                     , rlRemaining = remainingInt
                     , rlReset = ununix resetInt
                     , rlServerTime = dateTime
                     , rlClientTime = error "will be set in updateRateLimit"
                     }

updateRateLimit :: TVar (Maybe RateLimit) -> Maybe RateLimit -> IO ()
updateRateLimit _ Nothing = return () -- don't update if couldn't parse headers
updateRateLimit tv (Just rl) = do
  now <- dateCurrent
  let rl' = rl { rlClientTime = now }
  atomically $ writeTVar tv $ Just rl'

waitForRateLimit :: (T.Text -> IO ()) -> Maybe RateLimit -> IO ()
waitForRateLimit _ Nothing = return ()
waitForRateLimit warn (Just rl) =
  when (rlRemaining rl <= 0) $ do
    now <- dateCurrent
    let microseconds = calculateWait now rl
    warn $ fmt "rate limiting: waiting {} microseconds" (Only microseconds)
    threadDelay microseconds

calculateWait :: DateTime -> RateLimit -> Int
calculateWait now rl =
  let (waitS, waitNS) = rlReset rl `timeDiffP` now
      (skewS, skewNS) = rlClientTime rl `timeDiffP` rlServerTime rl
      (Seconds s, NanoSeconds ns) = (waitS - skewS, waitNS - skewNS)
      microseconds = toInteger s * 1000000 + toInteger ns `quot` 1000
      limited = microseconds `min` 60000000 -- never wait more than a minute
  in fromIntegral $ limited `max` 0

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
        req' = urlEncodedBody params req
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
        req' = urlEncodedBody params req
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

  cycleLights cc sel states dir = do
    txt <- selectorsToTextThrowIO sel
    req <- endpoint cc ("lights/" <> txt <> "/cycle")
    let states' = encode $ object ["states" .= states, "direction" .= dir]
        req' = (jsonPut req states') { method = methodPost }
    unResultWrapper <$> performRequest cc req'

  closeConnection _ = return ()

endpoint :: CloudConnection -> T.Text -> IO Request
endpoint cc ep = do
  let url = T.unpack $ ccRoot cc <> ep
  req <- parseUrl url
  return $ addHeaders (ccUserAgent cc) (TE.encodeUtf8 $ toText $ ccToken cc) req

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

defaultUserAgent =
  UserAgentComponent pkg_name version (Just pkg_url)
  : map mkUac libraryVersions
  where mkUac (pkg, vers) = UserAgentComponent pkg (pv vers) Nothing
        pv txt = Version (map (read . T.unpack) $ T.splitOn "." txt) []

appJson = "application/json"

addHeaders :: B.ByteString -> B.ByteString -> Request -> Request
addHeaders userAgent lifxToken req =
  applyBasicAuth lifxToken "" $ (setCheckStatus req)
    { requestHeaders = (hUserAgent, userAgent) : accept : requestHeaders req
    }
  where accept = (hAccept, appJson)

addConType :: B.ByteString -> Request -> Request
addConType ct req = req
  { requestHeaders = contentType : requestHeaders req }
  where contentType = (hContentType, ct)

jsonPut :: Request -> L.ByteString -> Request
jsonPut req body = addConType appJson req'
  where req' = req { method = methodPut, requestBody = RequestBodyLBS body }

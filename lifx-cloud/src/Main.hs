{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
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
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.IO as TIO
import Data.Text.Format hiding (print)
import Data.Text.Format.Params
import Data.Vector hiding (takeWhile, mapM_, (++), map, forM)
import Data.Version
import System.IO

import Lifx
import Lifx.Cloud

fromRight = either error id

main = do
  lifxTokenStr <- readFile "/Users/ppelleti/.lifxToken"
  let lifxToken = fromRight $ fromText $ T.pack $ takeWhile (not . isSpace) lifxTokenStr
      cs = defaultCloudSettings { csToken = lifxToken }
  cc <- openCloudConnection cs
  lites <- listLights cc [SelAll] needEverything
  -- lites <- listScenes cc
  -- lites <- activateScene cc (fromRight $ fromText "2c969519-1d6a-4c93-a4d7-d099045726c9") 5
  {- lites <- setStates cc [(SelGroup $ fromRight $ fromText "Bedroom",
                             StateTransition (Just On) emptyColor 10)] -}
  {- lites <- effect cc (SelGroup $ fromRight $ fromText "Bedroom")
              defaultEffect { eType = Pulse, eColor = red, eCycles = 5 } -}
  {-
  lites <- cycleLights cc (SelGroup $ fromRight $ fromText "Bedroom")
           [ StateTransition (Just On) red 2
           , StateTransition (Just On) yellow 2
           , StateTransition (Just On) green 2
           , StateTransition (Just On) blue 2
           , StateTransition (Just On) pink 2
           ]
  -}
  -- lites <- togglePower cc (SelGroup $ fromRight $ fromText "Bedroom") 1
  -- lites <- listLights cc (SelGroup $ fromRight $ fromText "Phelps") needEverything
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

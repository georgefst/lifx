{-# LANGUAGE CPP, OverloadedStrings #-}

module System.Hardware.Lifx.Cloud.Preprocessor
  ( setCheckStatus
  , createRequest
  ) where

import qualified Data.Text as T
import Network.HTTP.Client

setCheckStatus :: Request -> Request
setCheckStatus = id

createRequest :: String -> IO Request
createRequest = parseRequest

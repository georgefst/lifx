{-# LANGUAGE CPP, OverloadedStrings #-}

module System.Hardware.Lifx.Cloud.Preprocessor (libraryVersions) where

import qualified Data.Text as T

libraryVersions :: [(T.Text, T.Text)]
libraryVersions =
  [ ( "lifx-lowlevel" , VERSION_lifx_lowlevel )
  , ( "http-client" , VERSION_http_client )
  , ( "http-client-tls" , VERSION_http_client_tls )
  ]

{-# LANGUAGE CPP, OverloadedStrings #-}

module Lifx.Cloud.Preprocessor (libraryVersions) where

import qualified Data.Text as T

libraryVersions :: [(T.Text, T.Text)]
libraryVersions =
  [ ( "lifx-base" , VERSION_lifx_base )
  , ( "http-client" , VERSION_http_client )
  , ( "http-client-tls" , VERSION_http_client_tls )
  ]

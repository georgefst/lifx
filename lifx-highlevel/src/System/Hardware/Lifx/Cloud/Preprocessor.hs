{-# LANGUAGE CPP, OverloadedStrings #-}

module System.Hardware.Lifx.Cloud.Preprocessor
  ( libraryVersions
  , setCheckStatus
  ) where

import qualified Data.Text as T
import Network.HTTP.Client

libraryVersions :: [(T.Text, T.Text)]
libraryVersions =
  [ ( "lifx-lowlevel" , VERSION_lifx_lowlevel )
  , ( "http-client" , VERSION_http_client )
  , ( "http-client-tls" , VERSION_http_client_tls )
  ]

setCheckStatus :: Request -> Request
#if MIN_VERSION_http_client(0,5,0)
setCheckStatus = id
#else
setCheckStatus req = req { checkStatus = cstat }
  where cstat _ _ _ = Nothing
#endif

{-# LANGUAGE OverloadedStrings #-}

module Prosper.Internal.Request where

import           Network.Http.Client
import           OpenSSL
import           System.IO.Streams     (InputStream)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C
import           Data.Monoid           ((<>))

import           Prosper.User

apiUrl :: ByteString
apiUrl = "api.prosper.com"

issueRequest
    :: User
    -> ByteString
    -> Method
    -> ContentType
    -> (Response -> InputStream ByteString -> IO a)
    -> [(ByteString, ByteString)]
    -> IO a
issueRequest (User user pass) url method ct handler params = withOpenSSL $ do
    ctx <- baselineContextSSL
    con <- openConnectionSSL ctx apiUrl 443
    req <- buildRequest $ do
        http method $ "/api/" <> url
        setAuthorizationBasic user pass
        setContentType ct
    sendRequest con req (setBody params)
    resp <- receiveResponse con handler
    closeConnection con
    return resp
  where
    setBody [] = emptyBody
    setBody xs = encodedFormBody xs

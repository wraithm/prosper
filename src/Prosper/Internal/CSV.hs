{-# LANGUAGE OverloadedStrings #-}

module Prosper.Internal.CSV
    ( csvGet
    , csvGetStream
    ) where

import           Network.Http.Client
import           System.IO.Streams     (InputStream)

import           Data.ByteString       (ByteString)

import           Prosper.Money
import           Prosper.User
import           Prosper.Internal.Request

-- | Make a GET request to Prosper's CSV api, return the raw CSV data
csvGet
    :: User -- ^ The user name and password for the prosper user
    -> ByteString -- ^ The name of the API service, relative to the API url
    -> IO ByteString -- ^ Raw CSV response
csvGet userInfo url = issueRequest userInfo url GET "text/csv" concatHandler []

-- | Make a GET request to Prosper's CSV api, return the raw CSV data
csvGetStream
    :: User -- ^ The user name and password for the prosper user
    -> ByteString -- ^ The name of the API service, relative to the API url
    -> (InputStream ByteString -> IO a)
    -> IO a -- ^ Raw CSV response
csvGetStream userInfo url handler =
    issueRequest userInfo url GET "text/csv" (\_ is -> handler is) []

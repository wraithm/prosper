{-# LANGUAGE OverloadedStrings #-}

module Prosper.Internal.JSON
    ( jsonGet
    , jsonGetHandler
    , investRequest
    ) where

import           Network.Http.Client
import           System.IO.Streams        (InputStream)

import           Data.Aeson               (FromJSON)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as C
import           Data.Monoid              ((<>))

import           Prosper.Internal.Request
import           Prosper.Invest
import           Prosper.Money
import           Prosper.User

-- | Make a GET request to Prosper's JSON api, return the parsed JSON data
jsonGet
    :: FromJSON a
    => User -- ^ The user name and password for the prosper user
    -> ByteString -- ^ The name of the API service, relative to the API url
    -> IO a -- ^ JSON response
jsonGet userInfo url =
    issueRequest userInfo url GET "application/json" jsonHandler []

jsonGetHandler
    :: FromJSON a
    => User -- ^ The user name and password for the prosper user
    -> ByteString -- ^ The name of the API service, relative to the API url
    -> (Response -> InputStream ByteString -> IO a)
    -> IO a -- ^ JSON response
jsonGetHandler userInfo url handler =
    issueRequest userInfo url GET "application/json" handler []

investRequest
    :: User
    -> Int -- ^ The listing id
    -> Money -- ^ Amount
    -> IO InvestResponse -- ^ JSON response from Prosper
investRequest userInfo listingId amount =
    issueRequest userInfo "Invest" POST "application/x-www-form-urlencoded" jsonHandler
        [ ("listingId", C.pack $ show listingId)
        , ("amount", C.pack $ show amount)
        ]

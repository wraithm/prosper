{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Prosper.Commands
    ( invest
    , account
    , allListings
    , notes
    , listingFromNote

    , listingCSV

    , AccountException (..)
    , UnauthorizedException (..)
    ) where

import           Control.Exception     (Exception, throwIO)
import           Control.Monad         (when)

import           Data.ByteString.Char8 as C
import           Data.Maybe            (listToMaybe)
import           Data.Monoid           ((<>))
import           Data.Typeable         (Typeable)
import           Data.Vector           (Vector)

import           Network.Http.Client
import           System.IO.Streams     (InputStream)

import           Prosper.Account
import           Prosper.Internal.JSON
import           Prosper.Internal.CSV
import           Prosper.Invest
import           Prosper.Listing
import           Prosper.Money
import           Prosper.Note
import           Prosper.User

-- | An investment request. This requires a 'User', an amount of 'Money', and a 'Listing'.
invest :: User -> Money -> Listing -> IO InvestResponse
invest user amt l = investRequest user (listingId l) amt

-- | Request 'Account' information from Prosper
account :: User -> IO Account
account user = jsonGetHandler user "Account" accountHandler
  where
    accountHandler resp is = do
        let statusCode = getStatusCode resp
            statusMsg = getStatusMessage resp
        when (statusCode == 500) $
            throwIO (AccountException statusMsg)
        when (statusCode == 401) $
            throwIO (UnauthorizedException statusMsg)
        jsonHandler resp is

-- | Used as a hack around the 500 Critical Exception error
data AccountException = AccountException ByteString
    deriving (Typeable, Show)

instance Exception AccountException

-- | If unauthorized response, send
data UnauthorizedException = UnauthorizedException ByteString
    deriving (Typeable, Show)

instance Exception UnauthorizedException

-- | Request notes for a Prosper user
notes :: User -> IO (Vector Note)
notes user = jsonGet user "Notes"

-- | Given a 'Note', look up the associated 'Listing' by the ListingId
listingFromNote :: User -> Note -> IO (Maybe Listing)
listingFromNote user (Note { listingNumber = lid }) = do
    ls <- jsonGet user command
    return $ listToMaybe ls
  where
    command = "ListingsHistorical?$filter=ListingNumber eq " <> C.pack (show lid)

-- | Send a request to the Listings end-point at Prosper
allListings :: User -> IO (Vector Listing)
allListings user = jsonGet user "Listings"

-- | Request a particular 'Listing' via the CSV endpoint
listingCSV :: User -> Listing -> (InputStream ByteString -> IO a) -> IO a
listingCSV user l = csvGetStream user url
  where
    lid = listingId l
    url = "Listings?$filter=ListingNumber%20eq%20" <> C.pack (show lid)

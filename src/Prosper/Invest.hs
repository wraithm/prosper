{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Prosper.Invest
    ( InvestStatus (..)
    , InvestMessage (..)
    , InvestResponse (..)
    ) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Monad       (mzero)

import           Data.Aeson          hiding (Error, Success)
import           Data.Serialize
import           Data.Text.Read      as R

import           GHC.Generics

import           Prosper.Money

-- | Status of an invest request
data InvestStatus
    = Success
    | Failed
    | Error
    | PartialSuccess
    deriving (Show, Eq, Read, Generic)

-- | Status of Invest request response JSON parser
instance FromJSON InvestStatus where
    parseJSON (String "SUCCESS") = pure Success
    parseJSON (String "FAILED") = pure Failed
    parseJSON (String "ERROR") = pure Error
    parseJSON (String "PARTIAL_SUCCESS") = pure PartialSuccess
    parseJSON _ = mzero

instance Serialize InvestStatus where

-- | Message associated with an error
data InvestMessage
    = NoError
    | InternalError
    | InvestedAmountLessThanRequested
    | ListingNotAvailable
    | ListingNotFound
    | InsufficientFunds
    | ListingClosedBeforeBidPlaced
    | ServerBusy
    | SuitabilityRequirementsNotMet
    | InvestedAmountLessThanMinimumRequired
    | OtherError -- ^ Not in the prosper docs, this is a catch all
    deriving (Show, Eq, Read, Generic)

-- | Parser for message in an error
instance FromJSON InvestMessage where
    parseJSON (String "NO_ERROR") = pure NoError
    parseJSON (String "INTERNAL_ERROR") = pure InternalError
    parseJSON (String "INVESTED_AMOUNT_LESS_THAN_REQUESTED") =
        pure InvestedAmountLessThanRequested
    parseJSON (String "LISTING_NOT_AVAILABLE") = pure ListingNotAvailable
    parseJSON (String "LISTING_NOT_FOUND") = pure ListingNotFound
    parseJSON (String "INSUFFICIENT_FUNDS") = pure InsufficientFunds
    parseJSON (String "NOT_ENOUGH_BALANCE_AVAILABLE") = pure InsufficientFunds -- Isn't in the Prosper Docs
    parseJSON (String "LISTING_CLOSED_BEFORE_BID_PLACED") =
        pure ListingClosedBeforeBidPlaced
    parseJSON (String "SERVER_BUSY") = pure ServerBusy
    parseJSON (String "SUITABILITY_REQUIREMENTS_NOT_MET") =
        pure SuitabilityRequirementsNotMet
    parseJSON (String "INVESTED_AMOUNT_LESS_THAN_MINIMUM_REQUIRED") =
        pure InvestedAmountLessThanMinimumRequired
    parseJSON _ = pure OtherError

instance Serialize InvestMessage where

-- | JSON response to an invest request
data InvestResponse = InvestResponse
    { investStatus    :: InvestStatus
    , requestedAmount :: Money
    , investListingId :: Int
    , investMessage   :: InvestMessage
    , amountInvested  :: Money
    } deriving (Show, Generic)

newtype Money' = MkMoney { unMoney :: Money } deriving (Eq, Generic)

instance Show Money' where
    show (MkMoney x) = show x

instance FromJSON Money' where
    parseJSON (String x) = readDouble x
      where
        readDouble y = case R.double y of
            Right (z,_) -> pure $ MkMoney z
            Left _ -> mzero
    parseJSON _ = mzero

-- | Parser for invest request response on api/Invest
instance FromJSON InvestResponse where
    parseJSON (Object v) = InvestResponse
        <$> v .: "Status"
        <*> (unMoney <$> v .: "RequestedAmount")
        <*> v .: "ListingId"
        <*> v .: "Message"
        <*> (unMoney <$> v .: "AmountInvested")
    parseJSON _ = mzero

instance Serialize InvestResponse where

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Prosper.Account
    ( Account (..)
    , emptyAccount
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (mzero)

import           Data.Aeson

import           GHC.Generics

import           Prosper.Money

-- | A data structure for holding account information for Prosper
data Account = Account
    { availableCash                   :: !Money
      -- ^ Available cash for investing
    , pendingInvestments              :: !Money
      -- ^ Amount of money invested that is pending origination
    , totPrincipalRecvdOnActiveNotes  :: !Money
      -- ^ Total principal payments received on active notes
    , totalInvestedOnActiveNotes      :: !Money
      -- ^ Total amount invested
    , outstandingPrincipalActiveNotes :: !Money
      -- ^ Total principal outstanding on active notes
    , -- | Total dollars held in investible cash,
      -- pending orders in the market, outstanding principal
      totalAccountValue               :: !Money
    , pendingInvestmentsSecondaryMkt  :: !Money
      -- ^ Investments on Folio
    , pendingQuickInvestOrders        :: !Money
      -- ^ Prosper Quick Invests
    } deriving (Show, Eq, Generic)

-- | An empty account. All values are zero.
emptyAccount :: Account
emptyAccount = Account 0 0 0 0 0 0 0 0

-- | Parser for account info
--
-- Located at api/Account
instance FromJSON Account where
    parseJSON (Object v) = Account
        <$> v .: "AvailableCashBalance"
        <*> v .: "PendingInvestmentsPrimaryMkt"
        <*> v .: "TotalPrincipalReceivedOnActiveNotes"
        <*> v .: "TotalAmountInvestedOnActiveNotes"
        <*> v .: "OutstandingPrincipalOnActiveNotes"
        <*> v .: "TotalAccountValue"
        <*> v .: "PendingInvestmentsSecondaryMkt"
        <*> v .: "PendingQuickInvestOrders"
    parseJSON _ = mzero

instance ToJSON Account where

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Prosper.Note 
    ( NoteStatus (..)
    , NoteDefaultReason (..)
    , Note (..)
    ) where

import           Control.Applicative (pure, (<$>), (<*>))

import           Data.Aeson
import           Data.Text

import           GHC.Generics

import           Prosper.Listing     (Rating (..))
import           Prosper.Money

data NoteStatus
    = OriginationDelayed
    | Current
    | ChargeOff
    | Defaulted
    | NoteCompleted
    | FinalPaymentInProgress
    | NoteCancelled
    deriving (Show, Eq)

instance ToJSON NoteStatus where
    toJSON OriginationDelayed = Number 0
    toJSON Current = Number 1
    toJSON ChargeOff = Number 2
    toJSON Defaulted = Number 3
    toJSON NoteCompleted = Number 4
    toJSON FinalPaymentInProgress = Number 5
    toJSON NoteCancelled = Number 6

instance FromJSON NoteStatus where
    parseJSON (Number 0) = pure OriginationDelayed
    parseJSON (Number 1) = pure Current
    parseJSON (Number 2) = pure ChargeOff
    parseJSON (Number 3) = pure Defaulted
    parseJSON (Number 4) = pure NoteCompleted
    parseJSON (Number 5) = pure FinalPaymentInProgress
    parseJSON (Number 6) = pure NoteCancelled
    parseJSON _ = fail "Could not parse NoteStatus"

data NoteDefaultReason
    = Delinquency
    | Bankruptcy
    | Deceased
    | Repurchased
    | PaidInFull
    | SettledInFull
    deriving (Show, Eq)

instance ToJSON NoteDefaultReason where
    toJSON Delinquency = Number 1
    toJSON Bankruptcy = Number 2
    toJSON Deceased = Number 3
    toJSON Repurchased = Number 4
    toJSON PaidInFull = Number 5
    toJSON SettledInFull = Number 6

instance FromJSON NoteDefaultReason where
    parseJSON (Number 1) = pure Delinquency
    parseJSON (Number 2) = pure Bankruptcy
    parseJSON (Number 3) = pure Deceased
    parseJSON (Number 4) = pure Repurchased
    parseJSON (Number 5) = pure PaidInFull
    parseJSON (Number 6) = pure SettledInFull
    parseJSON _ = fail "Could not parse NoteDefaultReason"

data Note = Note
    { loanNoteId          :: Text
    , listingNumber       :: Int
    , amountParticipation :: Money
    , totalAmountBorrowed :: Money
    , borrowerRate        :: Double
    , noteRating          :: Rating
    , term                :: Int
    , ageInMonths         :: Int
--     , originationDate :: Date
    , daysPastDue         :: Int
    , principalBalance    :: Money
    , principalRepaid     :: Money
    , interestPaid        :: Money
    , serviceFees         :: Money
    , prosperFees         :: Money
    , lateFees            :: Money
    -- groupLeaderReward
    , noteStatus          :: NoteStatus
    , noteDefaultReason   :: Maybe NoteDefaultReason
    , isSold              :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON Note where
    parseJSON (Object v) = Note
        <$> v .: "LoanNoteID"
        <*> v .: "ListingNumber"
        <*> v .: "AmountParticipation"
        <*> v .: "TotalAmountBorrowed"
        <*> v .: "BorrowerRate"
        <*> v .: "ProsperRating"
        <*> v .: "Term"
        <*> v .: "AgeInMonths"
        <*> v .: "DaysPastDue"
        <*> v .: "PrincipalBalance"
        <*> v .: "PrincipalRepaid"
        <*> v .: "InterestPaid"
        <*> v .: "ServiceFees"
        <*> v .: "ProsperFees"
        <*> v .: "LateFees"
        <*> v .: "NoteStatus"
        <*> v .:? "NoteDefaultReason"
        <*> v .: "IsSold"
    parseJSON _ = fail "Could not parse Note"

instance ToJSON Note where

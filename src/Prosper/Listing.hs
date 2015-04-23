{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Prosper.Listing
    ( Listing(..)
    , Offer(..)
    , Credit(..)
    , Rating(..)
    , Category(..)
    , Status(..)
    ) where

import           Control.Applicative (pure, (<$>), (<*>))
import           Control.Monad       (mzero)

import           Data.Aeson
import           Data.Text.Read      as R
import           Data.Typeable
import           GHC.Generics

import           Prosper.Money

data Listing = Listing
    { listingId       :: !Int
    , status          :: !Status
    , rating          :: !Rating -- ^ Letter score assigned by Prosper
    , score           :: !(Maybe Int) -- ^ Prosper score
    , category        :: !Category -- ^ The reason for the loan (e.g. Auto, Student, etc)

    -- Market data
    , amountRemaining :: !Money

    , offer           :: !Offer -- ^ Contract data
    , credit          :: !Credit -- ^ Credit data
    } deriving Show
--     investmentType :: (Fractional | Whole)

-- | Two 'Listing's are equivalent if their 'listingId's are equal
instance Eq Listing where
    (Listing { listingId = id1 }) == (Listing { listingId = id2 }) =
        id1 == id2

-- | Data related to the listing's offer and contract terms
data Offer = Offer
    { requestAmount  :: !Money
    , rate           :: !Double -- ^ Interest rate for the borrower
    , termInMonths   :: !Int -- ^ Integer number of months
    , yield          :: !Double
    , effectiveYield :: !Double
    , apr            :: !Double -- ^ APR for the borrower
    } deriving Show
--     estimatedLossRate :: Double -- Maybe Money?
--     estimatedReturn :: Double -- Maybe Money?
--     startDate       :: Date -- Add dates later
--     creationDate    :: Date
--     verificationStage :: Maybe Int -- Don't know what VerificationStage is...
--     Add WholeLoanStartDate and WholeLoanEndDate

-- | Data related to the credibility of the listing
data Credit = Credit
    { fico                     :: !Int
    , bankcardUtilization      :: !Double
    , isHomeowner              :: !Bool
    , debtToIncome             :: !Double
    , monthsEmployed           :: !(Maybe Int)
    , currentDelinquencies     :: !(Maybe Int)
    , amountDelinquent         :: !(Maybe Money)
    , openCreditLines          :: !(Maybe Int)
    , totOpenRevolvingAccts    :: !(Maybe Int)
    , revolvingBalance         :: !(Maybe Money)
    , revolvingAvailableCredit :: !(Maybe Int) -- ^ Percent
    , incomeRange              :: !(Maybe (Money, Money)) -- ^ It is possible that they're unemployed, that's Nothing
    , statedMonthlyIncome      :: !(Maybe Money)
    , currentCreditLines       :: !(Maybe Int)
    , nowDelinquentDerog       :: !(Maybe Int)
    , wasDelinquentDerog       :: !(Maybe Int)
    } deriving Show
--     firstCreditLine :: Date
--     creditLinesLast7Years :: Int
--     inquiriesLast6Months :: Int
--     delinquenciesLast7Years :: Int -- These are maybe too specific to Prosper
--     publicRecordsLast10Years :: Int
--     oldestTradeOpenDate :: Date

-- | Parse a 'Listing' from JSON
instance FromJSON Listing where
    parseJSON (Object v) = Listing
            <$> v .: "ListingNumber" -- Is listingid?

            -- Prosper-specific data
            <*> v .: "ListingStatus"
            <*> v .: "ProsperRating"
            <*> v .:? "ProsperScore"
            <*> v .: "ListingCategory"

            <*> v .: "AmountRemaining"

            <*> (Offer
            <$> v .: "ListingRequestAmount"
            <*> v .: "BorrowerRate"
            <*> v .: "ListingTerm"
            <*> v .: "LenderYield"
            <*> v .: "EffectiveYield"
            <*> v .: "BorrowerAPR")

            <*> (Credit
            <$> (unFico <$> v .: "FICOScore")
            <*> v .: "BankcardUtilization"
            <*> v .: "IsHomeowner"
            <*> v .: "DTIwProsperLoan"
            <*> v .:? "MonthsEmployed"
            <*> v .:? "CurrentDelinquencies"
            <*> v .:? "AmountDelinquent"
            <*> v .:? "OpenCreditLines"
            <*> v .:? "TotalOpenRevolvingAccounts"
            <*> v .:? "RevolvingBalance"
            <*> v .:? "RevolvingAvailablePercent"
            -- Prosper credit data
            <*> (unIR <$> v .: "IncomeRange")
            <*> v .:? "StatedMonthlyIncome"
            <*> v .:? "CurrentCreditLines"
            <*> v .:? "NowDelinquentDerog"
            <*> v .:? "WasDelinquentDerog")
    parseJSON _ = mzero

-- Prosper's API is really weird and has the FICO type as a String
-- in JSON.
newtype FICO = FICO { unFico :: Int } 
    deriving Eq

instance Show FICO where
    show (FICO x) = show x

instance FromJSON FICO where
    parseJSON (String s) = readInt s
      where
        readInt x = case R.decimal x of
            Right (y,_) -> pure $ FICO y
            Left _ -> mzero
    parseJSON _ = mzero

newtype IncomeRange = IR { unIR :: Maybe (Money, Money) } 
    deriving Eq

instance Show IncomeRange where
    show (IR (Just (l, h))) = "$" ++ show l ++ "-$" ++ show h
    show (IR Nothing) = "Not displayed or unemployed"

instance FromJSON IncomeRange where
    parseJSON (Number 0) = pure $ IR Nothing -- Not displayed
    parseJSON (Number 1) = pure $ IR (Just (0, 0)) -- Make $0
    parseJSON (Number 2) = pure $ IR (Just (1, 24999))
    parseJSON (Number 3) = pure $ IR (Just (25000, 49999))
    parseJSON (Number 4) = pure $ IR (Just (50000, 74999))
    parseJSON (Number 5) = pure $ IR (Just (75000, 99999))
    parseJSON (Number 6) = pure $ IR (Just (100000, 500000)) -- DOCTHIS 500,000 is just an arbitrary high value
    parseJSON (Number 7) = pure $ IR Nothing -- Unemployed
    parseJSON _ = pure $ IR Nothing

-- | Prosper ratings, 'AA' is the most credible. 'HR' is the least credible.
data Rating
    = HR
    | E
    | D
    | C
    | B
    | A
    | AA
    deriving (Show, Eq, Ord, Typeable, Generic, Read, Enum)

instance FromJSON Rating where
instance ToJSON Rating where

-- | The 'Category' of a loan is the type of loan.
-- This is basically copied verbatim from the Prosper API.
data Category
    = NotAvailable
    | DebtConsolidation
    | HomeImprovement
    | Business
    | PersonalLoan
    | StudentUse
    | Auto
    | Other
    | BabyAdoptionLoans
    | Boat
    | CosmeticProcedures
    | EngagementRingFinancing
    | GreenLoans
    | HouseholdExpenses
    | LargePurchases
    | MedicalDental
    | Motorcycle
    | RV
    | Taxes
    | Vacation
    | WeddingLoans
    deriving (Show, Eq, Typeable, Generic, Read)

instance FromJSON Category where
    parseJSON (Number 0) = pure NotAvailable
    parseJSON (Number 1) = pure DebtConsolidation
    parseJSON (Number 2) = pure HomeImprovement
    parseJSON (Number 3) = pure Business
    parseJSON (Number 4) = pure PersonalLoan
    parseJSON (Number 5) = pure StudentUse
    parseJSON (Number 6) = pure Auto
    parseJSON (Number 7) = pure Other
    parseJSON (Number 8) = pure BabyAdoptionLoans
    parseJSON (Number 9) = pure Boat
    parseJSON (Number 10) = pure CosmeticProcedures
    parseJSON (Number 11) = pure EngagementRingFinancing
    parseJSON (Number 12) = pure GreenLoans
    parseJSON (Number 13) = pure HouseholdExpenses
    parseJSON (Number 14) = pure LargePurchases
    parseJSON (Number 15) = pure MedicalDental
    parseJSON (Number 16) = pure Motorcycle
    parseJSON (Number 17) = pure RV
    parseJSON (Number 18) = pure Taxes
    parseJSON (Number 19) = pure Vacation
    parseJSON (Number 20) = pure WeddingLoans
    parseJSON _ = pure Other

instance ToJSON Category where
    toJSON NotAvailable = Number 0
    toJSON DebtConsolidation = Number 1
    toJSON HomeImprovement = Number 2
    toJSON Business = Number 3
    toJSON PersonalLoan = Number 4
    toJSON StudentUse = Number 5
    toJSON Auto = Number 6
    toJSON Other = Number 7
    toJSON BabyAdoptionLoans = Number 8
    toJSON Boat = Number 9
    toJSON CosmeticProcedures = Number 10
    toJSON EngagementRingFinancing = Number 11
    toJSON GreenLoans = Number 12
    toJSON HouseholdExpenses = Number 13
    toJSON LargePurchases = Number 14
    toJSON MedicalDental = Number 15
    toJSON Motorcycle = Number 16
    toJSON RV = Number 17
    toJSON Taxes = Number 18
    toJSON Vacation = Number 19
    toJSON WeddingLoans = Number 20

data Status
    = Active
    | Withdrawn
    | Expired
    | ListingCompleted
    | ListingCancelled
    | Pending
    deriving (Show, Eq, Typeable, Generic)

instance FromJSON Status where
    parseJSON (Number 2) = pure Active
    parseJSON (Number 4) = pure Withdrawn
    parseJSON (Number 5) = pure Expired
    parseJSON (Number 6) = pure ListingCompleted
    parseJSON (Number 7) = pure ListingCancelled
    parseJSON (Number 8) = pure Pending
    parseJSON _ = mzero

instance ToJSON Status where

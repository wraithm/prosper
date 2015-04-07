{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Prosper.User
    ( User(..)
    ) where

import           Control.Monad             (when)

import           Data.ByteString           (ByteString)

-- | User info for Prosper data
data User = User
    { username :: !ByteString -- ^ Username for Prosper
    , password :: !ByteString -- ^ Password for Prosper
    } deriving (Show, Eq)

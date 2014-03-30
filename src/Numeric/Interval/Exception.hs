{-# LANGUAGE DeriveDataTypeable #-}
module Numeric.Interval.Exception
  ( EmptyInterval(..)
  , AmbiguousComparison(..)
  ) where

import Control.Exception
import Data.Data

data EmptyInterval = EmptyInterval
  deriving (Eq,Ord,Typeable,Data)

instance Show EmptyInterval where
  show EmptyInterval = "empty interval"

instance Exception EmptyInterval

data AmbiguousComparison = AmbiguousComparison
  deriving (Eq,Ord,Typeable,Data)

instance Show AmbiguousComparison where
  show AmbiguousComparison = "ambiguous comparison"

instance Exception AmbiguousComparison

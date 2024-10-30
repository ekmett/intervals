{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Interval.NonEmpty
-- Copyright   :  (c) Edward Kmett 2010-2013
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  DeriveDataTypeable
--
-- Interval arithmetic
--
-----------------------------------------------------------------------------

module Numeric.Interval.NonEmpty
  ( Interval
  , (...)
  , interval
  , whole
  , singleton
  , member
  , notMember
  , elem
  , notElem
  , inf
  , sup
  , singular
  , width
  , midpoint
  , distance
  , intersection
  , hull
  , bisect
  , bisectIntegral
  , magnitude
  , mignitude
  , contains
  , isSubsetOf
  , certainly, (<!), (<=!), (==!), (/=!), (>=!), (>!)
  , possibly, (<?), (<=?), (==?), (/=?), (>=?), (>?)
  , clamp
  , inflate, deflate
  , scale, symmetric
  , idouble
  , ifloat
  , iquot
  , irem
  , idiv
  , imod
  ) where

import Numeric.Interval.NonEmpty.Internal
import Prelude ()

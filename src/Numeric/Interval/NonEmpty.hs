{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if defined(__GLASGOW_HASKELL) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE DeriveGeneric #-}
#endif
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
  , whole
  , singleton
  , elem
  , notElem
  , inf
  , sup
  , singular
  , width
  , midpoint
  , intersection
  , hull
  , bisect
  , bisectIntegral
  , magnitude
  , mignitude
  , contains
  , isSubsetOf
  , certainly, (<!), (<=!), (==!), (>=!), (>!)
  , possibly, (<?), (<=?), (==?), (>=?), (>?)
  , clamp
  , idouble
  , ifloat
  ) where

import Numeric.Interval.NonEmpty.Internal
import Prelude ()

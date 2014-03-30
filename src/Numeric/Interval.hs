-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Interval
-- Copyright   :  (c) Edward Kmett 2010-2014
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  DeriveDataTypeable
--
-- Interval arithmetic
-----------------------------------------------------------------------------
module Numeric.Interval
  ( Interval
  , (...)
  , interval
  , whole
  , empty
  , null
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
  , distance
  , contains
  , isSubsetOf
  , certainly, (<!), (<=!), (==!), (>=!), (>!)
  , possibly, (<?), (<=?), (==?), (>=?), (>?)
  , idouble
  , ifloat
  ) where

import Numeric.Interval.Internal
import Prelude ()

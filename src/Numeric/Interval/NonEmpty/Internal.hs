{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if defined(__GLASGOW_HASKELL) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE DeriveGeneric #-}
#endif
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Interval.NonEmpty.Internal
-- Copyright   :  (c) Edward Kmett 2010-2014
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  DeriveDataTypeable
--
-- Interval arithmetic
-----------------------------------------------------------------------------
module Numeric.Interval.NonEmpty.Internal
  ( Interval(..)
  , (...)
  , interval
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

import Control.Exception as Exception
import Data.Data
import Data.Foldable hiding (minimum, maximum, elem, notElem)
import Data.Function (on)
import Data.Monoid
#if defined(__GLASGOW_HASKELL) && __GLASGOW_HASKELL__ >= 704
import GHC.Generics
#endif
import Numeric.Interval.Exception
import Prelude hiding (null, elem, notElem)

-- $setup

data Interval a = I !a !a deriving
  ( Data
  , Typeable
#if defined(__GLASGOW_HASKELL) && __GLASGOW_HASKELL__ >= 704
  , Generic
#if __GLASGOW_HASKELL__ >= 706
  , Generic1
#endif
#endif
  )

instance Foldable Interval where
  foldMap f (I a b) = f a `mappend` f b
  {-# INLINE foldMap #-}

infix 3 ...

negInfinity :: Fractional a => a
negInfinity = (-1)/0
{-# INLINE negInfinity #-}

posInfinity :: Fractional a => a
posInfinity = 1/0
{-# INLINE posInfinity #-}

fmod :: RealFrac a => a -> a -> a
fmod a b = a - q*b where
  q = realToFrac (truncate $ a / b :: Integer)
{-# INLINE fmod #-}

-- | Create a non-empty interval, turning it around if necessary
(...) :: Ord a => a -> a -> Interval a
a ... b
  | a <= b = I a b
  | otherwise = I b a
{-# INLINE (...) #-}

-- | Try to create a non-empty interval.
interval :: Ord a => a -> a -> Maybe (Interval a)
interval a b
  | a <= b = Just $ I a b
  | otherwise = Nothing


-- | The whole real number line
--
-- >>> whole
-- -Infinity ... Infinity
whole :: Fractional a => Interval a
whole = I negInfinity posInfinity
{-# INLINE whole #-}

-- | A singleton point
--
-- >>> singleton 1
-- 1 ... 1
singleton :: a -> Interval a
singleton a = I a a
{-# INLINE singleton #-}

-- | The infinumum (lower bound) of an interval
--
-- >>> inf (1 ... 20)
-- 1
inf :: Interval a -> a
inf (I a _) = a
{-# INLINE inf #-}

-- | The supremum (upper bound) of an interval
--
-- >>> sup (1 ... 20)
-- 20
sup :: Interval a -> a
sup (I _ b) = b
{-# INLINE sup #-}

-- | Is the interval a singleton point?
-- N.B. This is fairly fragile and likely will not hold after
-- even a few operations that only involve singletons
--
-- >>> singular (singleton 1)
-- True
--
-- >>> singular (1.0 ... 20.0)
-- False
singular :: Ord a => Interval a -> Bool
singular (I a b) = a == b
{-# INLINE singular #-}

instance Eq a => Eq (Interval a) where
  (==) = (==!)
  {-# INLINE (==) #-}

instance Show a => Show (Interval a) where
  showsPrec n (I a b) =
    showParen (n > 3) $
      showsPrec 3 a .
      showString " ... " .
      showsPrec 3 b

-- | Calculate the width of an interval.
--
-- >>> width (1 ... 20)
-- 19
--
-- >>> width (singleton 1)
-- 0
width :: Num a => Interval a -> a
width (I a b) = b - a
{-# INLINE width #-}

-- | Magnitude
--
-- >>> magnitude (1 ... 20)
-- 20
--
-- >>> magnitude (-20 ... 10)
-- 20
--
-- >>> magnitude (singleton 5)
-- 5
magnitude :: (Num a, Ord a) => Interval a -> a
magnitude = sup . abs
{-# INLINE magnitude #-}

-- | \"mignitude\"
--
-- >>> mignitude (1 ... 20)
-- 1
--
-- >>> mignitude (-20 ... 10)
-- 0
--
-- >>> mignitude (singleton 5)
-- 5
mignitude :: (Num a, Ord a) => Interval a -> a
mignitude = inf . abs
{-# INLINE mignitude #-}

instance (Num a, Ord a) => Num (Interval a) where
  I a b + I a' b' = (a + a') ... (b + b')
  {-# INLINE (+) #-}
  I a b - I a' b' = (a - b') ... (b - a')
  {-# INLINE (-) #-}
  I a b * I a' b' =
    minimum [a * a', a * b', b * a', b * b']
    ...
    maximum [a * a', a * b', b * a', b * b']
  {-# INLINE (*) #-}
  abs x@(I a b)
    | a >= 0    = x
    | b <= 0    = negate x
    | otherwise = 0 ... max (- a) b
  {-# INLINE abs #-}

  signum = increasing signum
  {-# INLINE signum #-}

  fromInteger i = singleton (fromInteger i)
  {-# INLINE fromInteger #-}

-- | Bisect an interval at its midpoint.
--
-- >>> bisect (10.0 ... 20.0)
-- (10.0 ... 15.0,15.0 ... 20.0)
--
-- >>> bisect (singleton 5.0)
-- (5.0 ... 5.0,5.0 ... 5.0)
bisect :: Fractional a => Interval a -> (Interval a, Interval a)
bisect (I a b) = (I a m, I m b) where m = a + (b - a) / 2
{-# INLINE bisect #-}

bisectIntegral :: Integral a => Interval a -> (Interval a, Interval a)
bisectIntegral (I a b)
  | a == m || b == m = (I a a, I b b)
  | otherwise        = (I a m, I m b)
  where m = a + (b - a) `div` 2
{-# INLINE bisectIntegral #-}

-- | Nearest point to the midpoint of the interval.
--
-- >>> midpoint (10.0 ... 20.0)
-- 15.0
--
-- >>> midpoint (singleton 5.0)
-- 5.0
midpoint :: Fractional a => Interval a -> a
midpoint (I a b) = a + (b - a) / 2
{-# INLINE midpoint #-}

-- | Determine if a point is in the interval.
--
-- >>> elem 3.2 (1.0 ... 5.0)
-- True
--
-- >>> elem 5 (1.0 ... 5.0)
-- True
--
-- >>> elem 1 (1.0 ... 5.0)
-- True
--
-- >>> elem 8 (1.0 ... 5.0)
-- False
elem :: Ord a => a -> Interval a -> Bool
elem x (I a b) = x >= a && x <= b
{-# INLINE elem #-}

-- | Determine if a point is not included in the interval
--
-- >>> notElem 8 (1.0 ... 5.0)
-- True
--
-- >>> notElem 1.4 (1.0 ... 5.0)
-- False
notElem :: Ord a => a -> Interval a -> Bool
notElem x xs = not (elem x xs)
{-# INLINE notElem #-}

-- | 'realToFrac' will use the midpoint
instance Real a => Real (Interval a) where
  toRational (I ra rb) = a + (b - a) / 2 where
    a = toRational ra
    b = toRational rb
  {-# INLINE toRational #-}

instance Ord a => Ord (Interval a) where
  compare (I ax bx) (I ay by)
    | bx < ay = LT
    | ax > by = GT
    | bx == ay && ax == by = EQ
    | otherwise = Exception.throw AmbiguousComparison
  {-# INLINE compare #-}

  max (I a b) (I a' b') = max a a' ... max b b'
  {-# INLINE max #-}

  min (I a b) (I a' b') = min a a' ... min b b'
  {-# INLINE min #-}

-- @'divNonZero' X Y@ assumes @0 `'notElem'` Y@
divNonZero :: (Fractional a, Ord a) => Interval a -> Interval a -> Interval a
divNonZero (I a b) (I a' b') =
  minimum [a / a', a / b', b / a', b / b']
  ...
  maximum [a / a', a / b', b / a', b / b']

-- @'divPositive' X y@ assumes y > 0, and divides @X@ by [0 ... y]
divPositive :: (Fractional a, Ord a) => Interval a -> a -> Interval a
divPositive x@(I a b) y
  | a == 0 && b == 0 = x
  -- b < 0 || isNegativeZero b = negInfinity ... ( b / y)
  | b < 0 = negInfinity ... (b / y)
  | a < 0 = whole
  | otherwise = (a / y) ... posInfinity
{-# INLINE divPositive #-}

-- divNegative assumes y < 0 and divides the interval @X@ by [y ... 0]
divNegative :: (Fractional a, Ord a) => Interval a -> a -> Interval a
divNegative x@(I a b) y
  | a == 0 && b == 0 = - x -- flip negative zeros
  -- b < 0 || isNegativeZero b = (b / y) ... posInfinity
  | b < 0 = (b / y) ... posInfinity
  | a < 0 = whole
  | otherwise = negInfinity ... (a / y)
{-# INLINE divNegative #-}

divZero :: (Fractional a, Ord a) => Interval a -> Interval a
divZero x@(I a b)
  | a == 0 && b == 0 = x
  | otherwise        = whole
{-# INLINE divZero #-}

instance (Fractional a, Ord a) => Fractional (Interval a) where
  -- TODO: check isNegativeZero properly
  x / y@(I a b)
    | 0 `notElem` y = divNonZero x y
    | iz && sz  = Exception.throw DivideByZero
    | iz        = divPositive x a
    |       sz  = divNegative x b
    | otherwise = divZero x
    where
      iz = a == 0
      sz = b == 0
  recip (I a b)   = on min recip a b ... on max recip a b
  {-# INLINE recip #-}
  fromRational r  = let r' = fromRational r in I r' r'
  {-# INLINE fromRational #-}

instance RealFrac a => RealFrac (Interval a) where
  properFraction x = (b, x - fromIntegral b)
    where
      b = truncate (midpoint x)
  {-# INLINE properFraction #-}
  ceiling x = ceiling (sup x)
  {-# INLINE ceiling #-}
  floor x = floor (inf x)
  {-# INLINE floor #-}
  round x = round (midpoint x)
  {-# INLINE round #-}
  truncate x = truncate (midpoint x)
  {-# INLINE truncate #-}

instance (RealFloat a, Ord a) => Floating (Interval a) where
  pi = singleton pi
  {-# INLINE pi #-}
  exp = increasing exp
  {-# INLINE exp #-}
  log (I a b) = (if a > 0 then log a else negInfinity) ... log b
  {-# INLINE log #-}
  cos x
    | width t >= pi = (-1) ... 1
    | inf t >= pi = - cos (t - pi)
    | sup t <= pi = decreasing cos t
    | sup t <= 2 * pi = (-1) ... cos ((pi * 2 - sup t) `min` inf t)
    | otherwise = (-1) ... 1
    where
      t = fmod x (pi * 2)
  {-# INLINE cos #-}
  sin x = cos (x - pi / 2)
  {-# INLINE sin #-}
  tan x
    | inf t' <= - pi / 2 || sup t' >= pi / 2 = whole
    | otherwise = increasing tan x
    where
      t = x `fmod` pi
      t' | t >= pi / 2 = t - pi
         | otherwise    = t
  {-# INLINE tan #-}
  asin (I a b) = I (if a <= -1 then -halfPi else asin a) (if b >= 1 then halfPi else asin b)
    where halfPi = pi / 2
  {-# INLINE asin #-}
  acos (I a b) = I (if b >= 1 then 0 else acos b) (if a < -1 then pi else acos a)
  {-# INLINE acos #-}
  atan = increasing atan
  {-# INLINE atan #-}
  sinh = increasing sinh
  {-# INLINE sinh #-}
  cosh x@(I a b)
    | b < 0  = decreasing cosh x
    | a >= 0 = increasing cosh x
    | otherwise  = I 0 $ cosh $ if - a > b
                                then a
                                else b
  {-# INLINE cosh #-}
  tanh = increasing tanh
  {-# INLINE tanh #-}
  asinh = increasing asinh
  {-# INLINE asinh #-}
  acosh (I a b) = I lo $ acosh b
    where lo | a <= 1 = 0
             | otherwise = acosh a
  {-# INLINE acosh #-}
  atanh (I a b) = I (if a <= - 1 then negInfinity else atanh a) (if b >= 1 then posInfinity else atanh b)
  {-# INLINE atanh #-}

-- | lift a monotone increasing function over a given interval
increasing :: (a -> b) -> Interval a -> Interval b
increasing f (I a b) = I (f a) (f b)

-- | lift a monotone decreasing function over a given interval
decreasing :: (a -> b) -> Interval a -> Interval b
decreasing f (I a b) = I (f b) (f a)

-- | We have to play some semantic games to make these methods make sense.
-- Most compute with the midpoint of the interval.
instance RealFloat a => RealFloat (Interval a) where
  floatRadix = floatRadix . midpoint

  floatDigits = floatDigits . midpoint
  floatRange = floatRange . midpoint
  decodeFloat = decodeFloat . midpoint
  encodeFloat m e = singleton (encodeFloat m e)
  exponent = exponent . midpoint
  significand x = min a b ... max a b
    where
      (_ ,em) = decodeFloat (midpoint x)
      (mi,ei) = decodeFloat (inf x)
      (ms,es) = decodeFloat (sup x)
      a = encodeFloat mi (ei - em - floatDigits x)
      b = encodeFloat ms (es - em - floatDigits x)
  scaleFloat n (I a b) = I (scaleFloat n a) (scaleFloat n b)
  isNaN (I a b) = isNaN a || isNaN b
  isInfinite (I a b) = isInfinite a || isInfinite b
  isDenormalized (I a b) = isDenormalized a || isDenormalized b
  -- contains negative zero
  isNegativeZero (I a b) = not (a > 0)
                  && not (b < 0)
                  && (  (b == 0 && (a < 0 || isNegativeZero a))
                     || (a == 0 && isNegativeZero a)
                     || (a < 0 && b >= 0))
  isIEEE _ = False

  atan2 = error "unimplemented"

-- TODO: (^), (^^) to give tighter bounds

-- | Calculate the intersection of two intervals.
--
-- >>> intersection (1 ... 10 :: Interval Double) (5 ... 15 :: Interval Double)
-- Just (5.0 ... 10.0)
intersection :: (Fractional a, Ord a) => Interval a -> Interval a -> Maybe (Interval a)
intersection x@(I a b) y@(I a' b')
  | x /=! y   = Nothing
  | otherwise = Just $ I (max a a') (min b b')
{-# INLINE intersection #-}

-- | Calculate the convex hull of two intervals
--
-- >>> hull (0 ... 10 :: Interval Double) (5 ... 15 :: Interval Double)
-- 0.0 ... 15.0
--
-- >>> hull (15 ... 85 :: Interval Double) (0 ... 10 :: Interval Double)
-- 0.0 ... 85.0
hull :: Ord a => Interval a -> Interval a -> Interval a
hull (I a b) (I a' b') = I (min a a') (max b b')
{-# INLINE hull #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<' y@
--
-- >>> (5 ... 10 :: Interval Double) <! (20 ... 30 :: Interval Double)
-- True
--
-- >>> (5 ... 10 :: Interval Double) <! (10 ... 30 :: Interval Double)
-- False
--
-- >>> (20 ... 30 :: Interval Double) <! (5 ... 10 :: Interval Double)
-- False
(<!)  :: Ord a => Interval a -> Interval a -> Bool
I _ bx <! I ay _ = bx < ay
{-# INLINE (<!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<=' y@
--
-- >>> (5 ... 10 :: Interval Double) <=! (20 ... 30 :: Interval Double)
-- True
--
-- >>> (5 ... 10 :: Interval Double) <=! (10 ... 30 :: Interval Double)
-- True
--
-- >>> (20 ... 30 :: Interval Double) <=! (5 ... 10 :: Interval Double)
-- False
(<=!) :: Ord a => Interval a -> Interval a -> Bool
I _ bx <=! I ay _ = bx <= ay
{-# INLINE (<=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '==' y@
--
-- Only singleton intervals or empty intervals can return true
--
-- >>> (singleton 5 :: Interval Double) ==! (singleton 5 :: Interval Double)
-- True
--
-- >>> (5 ... 10 :: Interval Double) ==! (5 ... 10 :: Interval Double)
-- False
(==!) :: Eq a => Interval a -> Interval a -> Bool
I ax bx ==! I ay by = bx == ay && ax == by
{-# INLINE (==!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '/=' y@
--
-- >>> (5 ... 15 :: Interval Double) /=! (20 ... 40 :: Interval Double)
-- True
--
-- >>> (5 ... 15 :: Interval Double) /=! (15 ... 40 :: Interval Double)
-- False
(/=!) :: Ord a => Interval a -> Interval a -> Bool
I ax bx /=! I ay by = bx < ay || ax > by
{-# INLINE (/=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>' y@
--
-- >>> (20 ... 40 :: Interval Double) >! (10 ... 19 :: Interval Double)
-- True
--
-- >>> (5 ... 20 :: Interval Double) >! (15 ... 40 :: Interval Double)
-- False
(>!)  :: Ord a => Interval a -> Interval a -> Bool
I ax _ >! I _ by = ax > by
{-# INLINE (>!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>=' y@
--
-- >>> (20 ... 40 :: Interval Double) >=! (10 ... 20 :: Interval Double)
-- True
--
-- >>> (5 ... 20 :: Interval Double) >=! (15 ... 40 :: Interval Double)
-- False
(>=!) :: Ord a => Interval a -> Interval a -> Bool
I ax _ >=! I _ by = ax >= by
{-# INLINE (>=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x `op` y@
certainly :: Ord a => (forall b. Ord b => b -> b -> Bool) -> Interval a -> Interval a -> Bool
certainly cmp l r
    | lt && eq && gt = True
    | lt && eq       = l <=! r
    | lt &&       gt = l /=! r
    | lt             = l <!  r
    |       eq && gt = l >=! r
    |       eq       = l ==! r
    |             gt = l >!  r
    | otherwise      = False
    where
        lt = cmp False True
        eq = cmp True True
        gt = cmp True False
{-# INLINE certainly #-}

-- | Check if interval @X@ totally contains interval @Y@
--
-- >>> (20 ... 40 :: Interval Double) `contains` (25 ... 35 :: Interval Double)
-- True
--
-- >>> (20 ... 40 :: Interval Double) `contains` (15 ... 35 :: Interval Double)
-- False
contains :: Ord a => Interval a -> Interval a -> Bool
contains (I ax bx) (I ay by) = ax <= ay && by <= bx
{-# INLINE contains #-}

-- | Flipped version of `contains`. Check if interval @X@ a subset of interval @Y@
--
-- >>> (25 ... 35 :: Interval Double) `isSubsetOf` (20 ... 40 :: Interval Double)
-- True
--
-- >>> (20 ... 40 :: Interval Double) `isSubsetOf` (15 ... 35 :: Interval Double)
-- False
isSubsetOf :: Ord a => Interval a -> Interval a -> Bool
isSubsetOf = flip contains
{-# INLINE isSubsetOf #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<' y@?
(<?) :: Ord a => Interval a -> Interval a -> Bool
I ax _ <? I _ by = ax < by
{-# INLINE (<?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<=' y@?
(<=?) :: Ord a => Interval a -> Interval a -> Bool
I ax _ <=? I _ by = ax <= by
{-# INLINE (<=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '==' y@?
(==?) :: Ord a => Interval a -> Interval a -> Bool
I ax bx ==? I ay by = ax <= by && bx >= ay
{-# INLINE (==?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '/=' y@?
(/=?) :: Eq a => Interval a -> Interval a -> Bool
I ax bx /=? I ay by = ax /= by || bx /= ay
{-# INLINE (/=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>' y@?
(>?) :: Ord a => Interval a -> Interval a -> Bool
I _ bx >? I ay _ = bx > ay
{-# INLINE (>?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>=' y@?
(>=?) :: Ord a => Interval a -> Interval a -> Bool
I _ bx >=? I ay _ = bx >= ay
{-# INLINE (>=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x `op` y@?
possibly :: Ord a => (forall b. Ord b => b -> b -> Bool) -> Interval a -> Interval a -> Bool
possibly cmp l r
    | lt && eq && gt = True
    | lt && eq       = l <=? r
    | lt &&       gt = l /=? r
    | lt             = l <? r
    |       eq && gt = l >=? r
    |       eq       = l ==? r
    |             gt = l >? r
    | otherwise      = False
    where
        lt = cmp LT EQ
        eq = cmp EQ EQ
        gt = cmp GT EQ
{-# INLINE possibly #-}

-- | The nearest value to that supplied which is contained in the interval.
clamp :: Ord a => Interval a -> a -> a
clamp (I a b) x
  | x < a     = a
  | x > b     = b
  | otherwise = x

-- | id function. Useful for type specification
--
-- >>> :t idouble (1 ... 3)
-- idouble (1 ... 3) :: Interval Double
idouble :: Interval Double -> Interval Double
idouble = id

-- | id function. Useful for type specification
--
-- >>> :t ifloat (1 ... 3)
-- ifloat (1 ... 3) :: Interval Float
ifloat :: Interval Float -> Interval Float
ifloat = id

-- Bugs:
-- sin 1 :: Interval Double

default (Integer,Double)

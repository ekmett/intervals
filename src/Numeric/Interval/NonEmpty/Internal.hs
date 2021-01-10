{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 704
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

import Control.Exception as Exception
import Data.Data
#if __GLASGOW_HASKELL__ >= 704
import GHC.Generics
#endif
import Prelude hiding (null, elem, notElem)

import qualified Data.Semigroup

-- $setup
-- >>> import Test.QuickCheck.Arbitrary
-- >>> import Test.QuickCheck.Gen hiding (scale)
-- >>> import Test.QuickCheck.Property
-- >>> import Control.Applicative
-- >>> import Control.Exception
-- >>> :set -XNoMonomorphismRestriction
-- >>> :set -XExtendedDefaultRules
-- >>> default (Integer,Double)
-- >>> instance (Ord a, Arbitrary a) => Arbitrary (Interval a) where arbitrary = (...) <$> arbitrary <*> arbitrary
-- >>> let memberOf xs = sized $ \n -> case n of { 0 -> pure $ inf xs; 1 -> pure $ sup xs; _ -> choose (inf xs, sup xs); }
-- >>> let conservative sf f xs = forAll (choose (inf xs, sup xs)) $ \x -> (sf x) `member` (f xs)
-- >>> let conservative2 sf f xs ys = forAll ((,) <$> choose (inf xs, sup xs) <*> choose (inf ys, sup ys)) $ \(x,y) -> (sf x y) `member` (f xs ys)
-- >>> let conservativeExceptNaN sf f xs = forAll (choose (inf xs, sup xs)) $ \x -> isNaN (sf x) || (sf x) `member` (f xs)
-- >>> let compose2 = fmap . fmap
-- >>> let commutative op a b = (a `op` b) == (b `op` a)
--
-- -- Eta expansion needed for GHC-7.6
-- >>> :set -fno-warn-deprecations
-- >>> let elem x xs = Numeric.Interval.NonEmpty.Internal.elem x xs
-- >>> let notElem x xs = Numeric.Interval.NonEmpty.Internal.notElem x xs

data Interval a = I !a !a deriving
  ( Eq, Ord
  , Data
  , Typeable
#if __GLASGOW_HASKELL__ >= 704
  , Generic
#if __GLASGOW_HASKELL__ >= 706
  , Generic1
#endif
#endif
  )

-- | 'Data.Semigroup.<>' is 'hull'
instance Ord a => Data.Semigroup.Semigroup (Interval a) where
  (<>) = hull

infix 3 ...

negInfinity :: Fractional a => a
negInfinity = (-1)/0
{-# INLINE negInfinity #-}

posInfinity :: Fractional a => a
posInfinity = 1/0
{-# INLINE posInfinity #-}

-- the sign of a number, but as an Ordering so that we can pattern match over it.
-- GT means greater than zero, etc.
signum' :: (Ord a, Num a) => a -> Ordering
signum' x = compare x 0

-- arguments are period, range, derivative, function, and interval
-- we require that each period of the function include precisely one local minimum and one local maximum
periodic :: (Num a, Ord a) => a -> Interval a -> (a -> Ordering) -> (a -> a) -> Interval a -> Interval a
periodic p r _ _ x | width x > p = r
periodic _ r d f (I a b) = periodic' r (d a) (d b) (f a) (f b)

-- arguments are global range, derivatives at endpoints, values at endpoints
periodic' :: (Ord a) => Interval a -> Ordering -> Ordering -> a -> a -> Interval a
periodic' r GT GT a b | a <= b = I a b -- stays in increasing zone
                      | otherwise = r  -- goes from increasing zone, all the way through decreasing zone, and back to increasing zone
periodic' r LT LT a b | a >= b = I b a -- stays in decreasing zone
                      | otherwise = r  -- goes from decreasing zone, all the way through increasing zone, and back to decreasing zone
periodic' r GT _  a b = I (min a b) (sup r) -- was going up, started going down
periodic' r LT _  a b = I (inf r) (max a b) -- was going down, started going up
periodic' r EQ GT a b | a < b = I a b -- stays in increasing zone
                      | otherwise = r -- goes from increasing zone, all the way through decreasing zone, and back to increasing zone
periodic' r EQ LT a b | a > b = I b a -- stays in decreasing zone
                      | otherwise = r -- goes from decreasing zone, all the way through increasing zone, and back to decreasing zone
periodic' _ _  _  a b = a ... b -- precisely begins and ends at local extremes, so it's either a singleton or whole

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
--
-- prop> (x :: Double) `elem` whole
whole :: Fractional a => Interval a
whole = I negInfinity posInfinity
{-# INLINE whole #-}

-- | A singleton point
--
-- >>> singleton 1
-- 1 ... 1
--
-- prop> x `elem` (singleton x)
-- prop> x /= y ==> y `notElem` (singleton x)
singleton :: a -> Interval a
singleton a = I a a
{-# INLINE singleton #-}

-- | The infinumum (lower bound) of an interval
--
-- >>> inf (1 ... 20)
-- 1
--
-- prop> min x y == inf (x ... y)
-- prop> inf x <= sup x
inf :: Interval a -> a
inf (I a _) = a
{-# INLINE inf #-}

-- | The supremum (upper bound) of an interval
--
-- >>> sup (1 ... 20)
-- 20
--
-- prop> sup x `elem` x
-- prop> max x y == sup (x ... y)
-- prop> inf x <= sup x
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
--
-- prop> 0 <= width x
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
--
-- prop> 0 <= magnitude x
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
--
-- prop> 0 <= mignitude x
mignitude :: (Num a, Ord a) => Interval a -> a
mignitude = inf . abs
{-# INLINE mignitude #-}

-- | Num instance for intervals.
--
-- prop> conservative2 ((+) :: Double -> Double -> Double) (+)
-- prop> conservative2 ((-) :: Double -> Double -> Double) (-)
-- prop> conservative2 ((*) :: Double -> Double -> Double) (*)
-- prop> conservative (abs :: Double -> Double) abs
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
--
-- prop> let (a, b) = bisect (x :: Interval Double) in sup a == inf b
-- prop> let (a, b) = bisect (x :: Interval Double) in inf a == inf x
-- prop> let (a, b) = bisect (x :: Interval Double) in sup b == sup x
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
--
-- prop> midpoint x `elem` (x :: Interval Double)
midpoint :: Fractional a => Interval a -> a
midpoint (I a b) = a + (b - a) / 2
{-# INLINE midpoint #-}

-- | Hausdorff distance between intervals.
--
-- >>> distance (1 ... 7) (6 ... 10)
-- 0
--
-- >>> distance (1 ... 7) (15 ... 24)
-- 8
--
-- >>> distance (1 ... 7) (-10 ... -2)
-- 3
--
-- prop> commutative (distance :: Interval Double -> Interval Double -> Double)
-- prop> 0 <= distance x y
distance :: (Num a, Ord a) => Interval a -> Interval a -> a
distance i1 i2 = mignitude (i1 - i2)

-- | Determine if a point is in the interval.
--
-- >>> member 3.2 (1.0 ... 5.0)
-- True
--
-- >>> member 5 (1.0 ... 5.0)
-- True
--
-- >>> member 1 (1.0 ... 5.0)
-- True
--
-- >>> member 8 (1.0 ... 5.0)
-- False
member :: Ord a => a -> Interval a -> Bool
member x (I a b) = x >= a && x <= b
{-# INLINE member #-}

-- | Determine if a point is not included in the interval
--
-- >>> notMember 8 (1.0 ... 5.0)
-- True
--
-- >>> notMember 1.4 (1.0 ... 5.0)
-- False
notMember :: Ord a => a -> Interval a -> Bool
notMember x xs = not (member x xs)
{-# INLINE notMember #-}

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
elem = member
{-# INLINE elem #-}
{-# DEPRECATED elem "Use `member` instead." #-}

-- | Determine if a point is not included in the interval
--
-- >>> notElem 8 (1.0 ... 5.0)
-- True
--
-- >>> notElem 1.4 (1.0 ... 5.0)
-- False
notElem :: Ord a => a -> Interval a -> Bool
notElem = notMember
{-# INLINE notElem #-}
{-# DEPRECATED notElem "Use `notMember` instead." #-}

-- | 'realToFrac' will use the midpoint
instance Real a => Real (Interval a) where
  toRational (I ra rb) = a + (b - a) / 2 where
    a = toRational ra
    b = toRational rb
  {-# INLINE toRational #-}

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

-- | Fractional instance for intervals.
--
-- prop> ys /= singleton 0 ==> conservative2 ((/) :: Double -> Double -> Double) (/) xs ys
-- prop> xs /= singleton 0 ==> conservative (recip :: Double -> Double) recip xs
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

-- | Transcendental functions for intervals.
--
-- prop> conservative (exp :: Double -> Double) exp
-- prop> conservativeExceptNaN (log :: Double -> Double) log
-- prop> conservative (sin :: Double -> Double) sin
-- prop> conservative (cos :: Double -> Double) cos
-- prop> conservative (tan :: Double -> Double) tan
-- prop> conservativeExceptNaN (asin :: Double -> Double) asin
-- prop> conservativeExceptNaN (acos :: Double -> Double) acos
-- prop> conservative (atan :: Double -> Double) atan
-- prop> conservative (sinh :: Double -> Double) sinh
-- prop> conservative (cosh :: Double -> Double) cosh
-- prop> conservative (tanh :: Double -> Double) tanh
-- prop> conservativeExceptNaN (asinh :: Double -> Double) asinh
-- prop> conservativeExceptNaN (acosh :: Double -> Double) acosh
-- prop> conservativeExceptNaN (atanh :: Double -> Double) atanh
--
-- >>> cos (0 ... (pi + 0.1))
-- -1.0 ... 1.0
instance (RealFloat a, Ord a) => Floating (Interval a) where
  pi = singleton pi
  {-# INLINE pi #-}
  exp = increasing exp
  {-# INLINE exp #-}
  log (I a b) = (if a > 0 then log a else negInfinity) ... (if b > 0 then log b else negInfinity)
  {-# INLINE log #-}
  sin = periodic (2 * pi) (symmetric 1) (signum' . cos)          sin
  cos = periodic (2 * pi) (symmetric 1) (signum' . negate . sin) cos
  tan = periodic pi       whole         (const GT)               tan -- derivative only has to have correct sign
  asin (I a b) = (asin' a) ... (asin' b)
    where
      asin' x | x >= 1 = halfPi
              | x <= -1 = -halfPi
              | otherwise = asin x
      halfPi = pi / 2
  {-# INLINE asin #-}
  acos (I a b) = (acos' a) ... (acos' b)
    where
      acos' x | x >= 1 = 0
              | x <= -1 = pi
              | otherwise = acos x
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
  acosh (I a b) = (acosh' a) ... (acosh' b)
    where
      acosh' x | x <= 1 = 0
               | otherwise = acosh x
  {-# INLINE acosh #-}
  atanh (I a b) = (atanh' a) ... (atanh' b)
    where
      atanh' x | x <= -1 = negInfinity
               | x >= 1 = posInfinity
               | otherwise = atanh x
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
intersection :: Ord a => Interval a -> Interval a -> Maybe (Interval a)
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
--
-- prop> conservative2 const hull
-- prop> conservative2 (flip const) hull
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
--
-- prop> (clamp xs y) `elem` xs
clamp :: Ord a => Interval a -> a -> a
clamp (I a b) x
  | x < a     = a
  | x > b     = b
  | otherwise = x

-- | Inflate an interval by enlarging it at both ends.
--
-- >>> inflate 3 (-1 ... 7)
-- -4 ... 10
--
-- >>> inflate (-2) (0 ... 4)
-- -2 ... 6
--
-- prop> inflate x i `contains` i
inflate :: (Num a, Ord a) => a -> Interval a -> Interval a
inflate x y = symmetric x + y

-- | Deflate an interval by shrinking it from both ends.
-- Note that in cases that would result in an empty interval, the result is a singleton interval at the midpoint.
--
-- >>> deflate 3.0 (-4.0 ... 10.0)
-- -1.0 ... 7.0
--
-- >>> deflate 2.0 (-1.0 ... 1.0)
-- 0.0 ... 0.0
deflate :: (Fractional a, Ord a) => a -> Interval a -> Interval a
deflate x i@(I a b) | a' <= b'  = I a' b'
                    | otherwise = singleton m
  where
    a' = a + x
    b' = b - x
    m = midpoint i

-- | Scale an interval about its midpoint.
--
-- >>> scale 1.1 (-6.0 ... 4.0)
-- -6.5 ... 4.5
--
-- >>> scale (-2.0) (-1.0 ... 1.0)
-- -2.0 ... 2.0
--
-- prop> abs x >= 1 ==> (scale (x :: Double) i) `contains` i
-- prop> forAll (choose (0,1)) $ \x -> abs x <= 1 ==> i `contains` (scale (x :: Double) i)
scale :: (Fractional a, Ord a) => a -> Interval a -> Interval a
scale x i = a ... b where
  h = x * width i / 2
  mid = midpoint i
  a = mid - h
  b = mid + h

-- | Construct a symmetric interval.
--
-- >>> symmetric 3
-- -3 ... 3
--
-- >>> symmetric (-2)
-- -2 ... 2
--
-- prop> x `elem` symmetric x
-- prop> 0 `elem` symmetric x
symmetric :: (Num a, Ord a) => a -> Interval a
symmetric x = negate x ... x

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

-- | an interval containing all x `quot` y
-- prop> forAll (memberOf xs) $ \ x -> forAll (memberOf ys) $ \ y -> 0 `notMember` ys ==> (x `quot` y) `member` (xs `iquot` ys)
-- prop> 0 `member` ys ==> ioProperty $ do z <- try (evaluate (xs `iquot` ys)); return $ z === Left DivideByZero
iquot :: Integral a => Interval a -> Interval a -> Interval a
iquot (I l u) (I l' u') =
  if l' <= 0 && 0 <= u' then throw DivideByZero else I
    (minimum [a `quot` b | a <- [l,u], b <- [l',u']])
    (maximum [a `quot` b | a <- [l,u], b <- [l',u']])

-- | an interval containing all x `rem` y
-- prop> forAll (memberOf xs) $ \ x -> forAll (memberOf ys) $ \ y -> 0 `notMember` ys ==> (x `rem` y) `member` (xs `irem` ys)
-- prop> 0 `member` ys ==> ioProperty $ do z <- try (evaluate (xs `irem` ys)); return $ z === Left DivideByZero
irem :: Integral a => Interval a -> Interval a -> Interval a
irem (I l u) (I l' u') =
  if l' <= 0 && 0 <= u' then throw DivideByZero else I
    (minimum [0, signum l * (abs u' - 1), signum l * (abs l' - 1)])
    (maximum [0, signum u * (abs u' - 1), signum u * (abs l' - 1)])

-- | an interval containing all x `div` y
-- prop> forAll (memberOf xs) $ \ x -> forAll (memberOf ys) $ \ y -> 0 `notMember` ys ==> (x `div` y) `member` (xs `idiv` ys)
-- prop> 0 `member` ys ==> ioProperty $ do z <- try (evaluate (xs `idiv` ys)); return $ z === Left DivideByZero
idiv :: Integral a => Interval a -> Interval a -> Interval a
idiv (I l u) (I l' u') =
  if l' <= 0 && 0 <= u' then throw DivideByZero else I
    (min (l `Prelude.div` max 1 l') (u `Prelude.div` min (-1) u'))
    (max (u `Prelude.div` max 1 l') (l `Prelude.div` min (-1) u'))

-- | an interval containing all x `mod` y
-- prop> forAll (memberOf xs) $ \ x -> forAll (memberOf ys) $ \ y -> 0 `notMember` ys ==> (x `mod` y) `member` (xs `imod` ys)
-- prop> 0 `member` ys ==> ioProperty $ do z <- try (evaluate (xs `imod` ys)); return $ z === Left DivideByZero
imod :: Integral a => Interval a -> Interval a -> Interval a
imod _ (I l' u') =
  if l' <= 0 && 0 <= u' then throw DivideByZero else
    I (min (l'+1) 0) (max 0 (u'-1))

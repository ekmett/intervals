{-# LANGUAGE Rank2Types, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Interval
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Interval arithmetic
--
-----------------------------------------------------------------------------

module Numeric.Interval 
    ( Interval(..)
    , (...)
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
    , intersection
    , hull
    , bisection
    , magnitude
    , mignitude
    , contains
    , isSubsetOf
    , certainly, (<!), (<=!), (==!), (>=!), (>!)
    , possibly, (<?), (<=?), (==?), (>=?), (>?)
    , idouble 
    , ifloat 
    ) where

import Prelude hiding (null, elem, notElem)
import Numeric.Extras
import Data.Function (on)

data Interval a = I !a !a

infix 3 ...

negInfinity :: Fractional a => a
negInfinity = (-1)/0 
{-# INLINE negInfinity #-}

posInfinity :: Fractional a => a
posInfinity = 1/0
{-# INLINE posInfinity #-}

nan :: Fractional a => a 
nan = 0/0

-- | The rule of thumb is you should only use this to construct using values
-- that you took out of the interval. Otherwise, use I, to force rounding
(...) :: a -> a -> Interval a 
a ... b = I a b
{-# INLINE (...) #-}

-- | The whole real number line
whole :: Fractional a => Interval a 
whole = negInfinity ... posInfinity
{-# INLINE whole #-}

-- | An empty interval
empty :: Fractional a => Interval a 
empty = nan ... nan
{-# INLINE empty #-}

-- | negation handles NaN properly
null :: Ord a => Interval a -> Bool
null x = not (inf x <= sup x)
{-# INLINE null #-}

-- | A singleton point
singleton :: a -> Interval a 
singleton a = a ... a
{-# INLINE singleton #-}

-- | The infinumum (lower bound) of an interval
inf :: Interval a -> a
inf (I a _) = a
{-# INLINE inf #-}

-- | The supremum (upper bound) of an interval
sup :: Interval a -> a
sup (I _ b) = b
{-# INLINE sup #-}

-- | Is the interval a singleton point? 
-- N.B. This is fairly fragile and likely will not hold after
-- even a few operations that only involve singletons
singular :: Ord a => Interval a -> Bool
singular x = not (null x) && inf x == sup x
{-# INLINE singular #-}

instance Eq a => Eq (Interval a) where
    (==) = (==!) 

instance Show a => Show (Interval a) where
    showsPrec n (I a b) =   
        showParen (n > 3) $
            showsPrec 3 a .
            showString " ... " . 
            showsPrec 3 b

-- | Calculate the width of an interval.
width :: Num a => Interval a -> a
width (I a b) = b - a
{-# INLINE width #-}

-- | magnitude 
magnitude :: (Num a, Ord a) => Interval a -> a 
magnitude x = (max `on` abs) (inf x) (sup x)
{-# INLINE magnitude #-}

-- | "mignitude"
mignitude :: (Num a, Ord a) => Interval a -> a 
mignitude x = (min `on` abs) (inf x) (sup x)
{-# INLINE mignitude #-}

instance (Num a, Ord a) => Num (Interval a) where
    I a b + I a' b' = (a + a') ... (b + b')
    I a b - I a' b' = (a - b') ... (b - a')
    I a b * I a' b' = minimum [a * a', a * b', b * a', b * b'] 
                      ...
                      maximum [a * a', a * b', b * a', b * b']
    abs x@(I a b) 
        | a >= 0    = x 
        | b <= 0    = negate x
        | otherwise = max (- a) b ... b

    signum = increasing signum

    fromInteger i = singleton (fromInteger i)

-- | Bisect an interval at its midpoint.
bisection :: Fractional a => Interval a -> (Interval a, Interval a)
bisection x = (inf x ... m, m ... sup x)
    where m = midpoint x
{-# INLINE bisection #-}

-- | Nearest point to the midpoint of the interval.
midpoint :: Fractional a => Interval a -> a
midpoint x = inf x + (sup x - inf x) / 2
{-# INLINE midpoint #-}

elem :: Ord a => a -> Interval a -> Bool
elem x xs = x >= inf xs && x <= sup xs
{-# INLINE elem #-}

notElem :: Ord a => a -> Interval a -> Bool
notElem x xs = not (elem x xs)
{-# INLINE notElem #-}

-- | This means that realToFrac will use the midpoint

-- | What moron put an Ord instance requirement on Real!
instance Real a => Real (Interval a) where
    toRational x 
        | null x   = nan
        | otherwise = a + (b - a) / 2
        where
            a = toRational (inf x)
            b = toRational (sup x)

instance Ord a => Ord (Interval a) where
    compare x y 
        | sup x < inf y = LT
        | inf x > sup y = GT
        | sup x == inf y && inf x == sup y = EQ
        | otherwise = error "Numeric.Interval.compare: ambiguous comparison"
    min = minInterval
    max = maxInterval

-- @'divNonZero' X Y@ assumes @0 `'notElem'` Y@
divNonZero :: (Fractional a, Ord a) => Interval a -> Interval a -> Interval a 
divNonZero (I a b) (I a' b') = 
    minimum [a / a', a / b', b / a', b / b'] 
    `I`
    maximum [a / a', a / b', b / a', b / b']

-- @'divPositive' X y@ assumes y > 0, and divides @X@ by [0 ... y]
divPositive :: (Fractional a, Ord a) => Interval a -> a -> Interval a 
divPositive x@(I a b) y
    | a == 0 && b == 0 = x
    -- | b < 0 || isNegativeZero b = negInfinity `I` ( b / y)
    | b < 0 = negInfinity `I` ( b / y)
    | a < 0 = whole 
    | otherwise = (a / y) `I` posInfinity

-- divNegative assumes y < 0 and divides the interval @X@ by [y ... 0]
divNegative :: (Fractional a, Ord a) => Interval a -> a -> Interval a
divNegative x@(I a b) y
    | a == 0 && b == 0 = - x -- flip negative zeros
    -- | b < 0 || isNegativeZero b = (b / y) `I` posInfinity
    | b < 0 = (b / y) `I` posInfinity
    | a < 0 = whole
    | otherwise = negInfinity `I` (a / y)

divZero :: (Fractional a, Ord a) => Interval a -> Interval a
divZero x | inf x == 0 && sup x == 0 = x
          | otherwise = whole

instance (Fractional a, Ord a) => Fractional (Interval a) where
    -- TODO: check isNegativeZero properly
    x / y
        | 0 `notElem` y = divNonZero x y 
        | iz && sz  = empty -- division by 0
        | iz        = divPositive x (inf y)
        |       sz  = divNegative x (sup y)
        | otherwise = divZero x
        where 
            iz = inf y == 0
            sz = sup y == 0
    recip (I a b)   = on min recip a b ... on max recip a b
    fromRational r  = fromRational r ... fromRational r

instance RealFloat a => RealFrac (Interval a) where
    properFraction x = (b, x - fromIntegral b)
        where 
            b = truncate (midpoint x)
    ceiling x = ceiling (sup x)
    floor x = floor (inf x)
    round x = round (midpoint x)
    truncate x = truncate (midpoint x)

instance (RealExtras a, Ord a) => Floating (Interval a) where
    pi = singleton pi
    exp = increasing exp
    log (I a b) = (if a > 0 then log a else negInfinity) ... log b
    cos x 
        | null x = empty
        | width t >= pi = (-1) ... 1
        | inf t >= pi = - cos (t - pi)
        | sup t <= pi = decreasing cos t
        | sup t <= 2 * pi = (-1) ... cos ((pi * 2 - sup t) `min` inf t)
        | otherwise = (-1) ... 1
        where 
            t = fmod x (pi * 2)
    sin x 
        | null x = empty
        | otherwise = cos (x - pi / 2)
    tan x 
        | null x = empty
        | inf t' <= - pi / 2 || sup t' >= pi / 2 = whole
        | otherwise = increasing tan x
        where
            t = x `fmod` pi 
            t' | t >= pi / 2 = t - pi
               | otherwise    = t
    asin x@(I a b)
        | null x || b < -1 || a > 1 = empty
        | otherwise = 
            (if a <= -1 then -halfPi else asin a)
            `I`
            (if b >= 1 then halfPi else asin b)
        where
            halfPi = pi / 2
    acos x@(I a b)
        | null x || b < -1 || a > 1 = empty
        | otherwise = 
            (if b >= 1 then 0 else acos b)
            `I`
            (if a < -1 then pi else acos a)
    atan = increasing atan
    sinh = increasing sinh
    cosh x@(I a b)
        | null x = empty
        | b < 0  = decreasing cosh x
        | a >= 0 = increasing cosh x
        | otherwise  = I 0 $ cosh $ if - a > b
                                    then a 
                                    else b
    tanh = increasing tanh
    asinh = increasing asinh
    acosh x@(I a b)
        | null x || b < 1 = empty
        | otherwise = I lo $ acosh b
        where lo | a <= 1 = 0 
                 | otherwise = acosh a
    atanh x@(I a b)
        | null x || b < -1 || a > 1 = empty
        | otherwise =
                (if a <= - 1 then negInfinity else atanh a)
                `I`
                (if b >= 1 then posInfinity else atanh b)
    
-- | lift a monotone increasing function over a given interval 
increasing :: (a -> a) -> Interval a -> Interval a
increasing f (I a b) = I (f a) (f b)

-- | lift a monotone increasing function over a given interval 
decreasing :: (a -> a) -> Interval a -> Interval a
decreasing f (I a b) = I (f b) (f a)

-- | We have to play some semantic games to make these methods make sense.
-- Most compute with the midpoint of the interval.
instance RealExtras a => RealFloat (Interval a) where
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
    scaleFloat n x = scaleFloat n (inf x) ... scaleFloat n (sup x)
    isNaN x = isNaN (inf x) || isNaN (sup x)
    isInfinite x = isInfinite (inf x) || isInfinite (sup x)
    isDenormalized x = isDenormalized (inf x) || isDenormalized (sup x)
    -- contains negative zero
    isNegativeZero x = not (inf x > 0) 
                    && not (sup x < 0)
                    && (  (sup x == 0 && (inf x < 0 || isNegativeZero (inf x)))
                       || (inf x == 0 && isNegativeZero (inf x)) 
                       || (inf x < 0 && sup x >= 0))
    isIEEE x = isIEEE (inf x) && isIEEE (sup x)
    atan2 = error "unimplemented"

-- TODO: (^), (^^) to give tighter bounds
        
-- | Calculate the intersection of two intervals.
intersection :: (Fractional a, Ord a) => Interval a -> Interval a -> Interval a
intersection x@(I a b) y@(I a' b')
    | x /=! y = empty
    | otherwise = I (max a a') (min b b')
{-# INLINE intersection #-}

-- | Calculate the convex hull of two intervals
hull :: Ord a => Interval a -> Interval a -> Interval a
hull x@(I a b) y@(I a' b') 
    | null x = y
    | null y = x
    | otherwise = I (min a a') (max b b')
{-# INLINE hull #-}
    
instance RealExtras a => RealExtras (Interval a) where
    type C (Interval a) = C a
    fmod x y | null y = empty 
             | otherwise = r -- `intersection` bounds
        where 
            n :: Integer
            n = floor (inf x / if inf x < 0 then inf y else sup y)
            r = x - fromIntegral n * y 
            -- bounds | inf y >= 0 = y
            --        | otherwise = y `hull` negate y
    expm1 = increasing expm1
    log1p (I a b) = (if a > (-1) then log1p a else negInfinity) `I` log1p b
    hypot x y = hypot a a' `I` hypot b b'
        where
            I a b = abs x
            I a' b' = abs y
    cbrt = increasing cbrt
    erf = increasing erf

-- | For all @x@ in @X@, @y@ in @Y@. @x '<' y@
(<!)  :: Ord a => Interval a -> Interval a -> Bool
x <! y = sup x < inf y
{-# INLINE (<!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '<=' y@
(<=!) :: Ord a => Interval a -> Interval a -> Bool
x <=! y = sup x <= inf y
{-# INLINE (<=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '==' y@
(==!) :: Eq a => Interval a -> Interval a -> Bool
x ==! y = sup x == inf y && inf x == sup y
{-# INLINE (==!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '/=' y@
(/=!) :: Ord a => Interval a -> Interval a -> Bool
x /=! y = sup x < inf y || inf x > sup y
{-# INLINE (/=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>' y@
(>!)  :: Ord a => Interval a -> Interval a -> Bool
x >! y = inf x > sup y
{-# INLINE (>!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x '>=' y@
(>=!) :: Ord a => Interval a -> Interval a -> Bool
x >=! y = inf x >= sup y
{-# INLINE (>=!) #-}

-- | For all @x@ in @X@, @y@ in @Y@. @x `op` y@
certainly :: Ord a => (forall b. Ord b => b -> b -> Bool) -> Interval a -> Interval a -> Bool
certainly cmp l r 
    | lt && eq && gt = True
    | lt && eq       = l <=! r
    | lt &&       gt = l /=! r
    | lt             = l <! r 
    |       eq && gt = l >=! r 
    |       eq       = l ==! r
    |             gt = l >! r
    | otherwise      = False
    where 
        lt = cmp LT EQ
        eq = cmp EQ EQ
        gt = cmp GT EQ
{-# INLINE certainly #-}

contains :: Ord a => Interval a -> Interval a -> Bool
contains x y = null y 
            || (not (null x) && inf x <= inf y && sup y <= sup x)
{-# INLINE contains #-}

isSubsetOf :: Ord a => Interval a -> Interval a -> Bool
isSubsetOf = flip contains

maxInterval :: Ord a => Interval a -> Interval a -> Interval a
maxInterval  (I a b) (I a' b') = I (max a a') (max b b')

minInterval :: Ord a => Interval a -> Interval a -> Interval a
minInterval  (I a b) (I a' b') = I (min a a') (min b b')

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<' y@?
(<?) :: Ord a => Interval a -> Interval a -> Bool
x <? y = inf x < sup y
{-# INLINE (<?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '<=' y@?
(<=?) :: Ord a => Interval a -> Interval a -> Bool
x <=? y = inf x <= sup y
{-# INLINE (<=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '==' y@?
(==?) :: Ord a => Interval a -> Interval a -> Bool
x ==? y = inf x <= sup y && sup x >= inf y
{-# INLINE (==?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '/=' y@?
(/=?) :: Eq a => Interval a -> Interval a -> Bool
x /=? y = inf x /= sup y || sup x /= inf y
{-# INLINE (/=?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>' y@?
(>?) :: Ord a => Interval a -> Interval a -> Bool
x >? y = sup x > inf y
{-# INLINE (>?) #-}

-- | Does there exist an @x@ in @X@, @y@ in @Y@ such that @x '>=' y@?
(>=?) :: Ord a => Interval a -> Interval a -> Bool
x >=? y = sup x >= inf y
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

idouble :: Interval Double -> Interval Double
idouble = id

ifloat :: Interval Float -> Interval Float
ifloat = id

-- Bugs:
-- sin 1 :: Interval Double

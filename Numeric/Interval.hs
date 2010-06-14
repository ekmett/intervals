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
    ( whole
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
    , certainly, (<!), (<=!), (==!), (>=!), (>!)
    , possibly, (<?), (<=?), (==?), (>=?), (>?)
    ) where

import Prelude hiding (null, elem, notElem)
import Numeric.Extras
import Numeric.Rounding
import Data.Function (on)

data Interval a = I (Round Down a) (Round Up a)

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
a ... b = I (Round a) (Round b)
{-# INLINE (...) #-}

-- | The whole real number line
whole :: Precision a => Interval a 
whole = negInfinity ... posInfinity
{-# INLINE whole #-}

-- | An empty interval
empty :: Precision a => Interval a 
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
inf (I (Round a) _) = a
{-# INLINE inf #-}

-- | The supremum (upper bound) of an interval
sup :: Interval a -> a
sup (I _ (Round b)) = b
{-# INLINE sup #-}

-- | Is the interval a singleton point? 
-- N.B. This is fairly fragile and likely will not hold after
-- even a few operations that only involve singletons
singular :: Ord a => Interval a -> Bool
singular x = not (null x) && inf x == sup x
{-# INLINE singular #-}

instance Precision a => Eq (Interval a) where
    (==) = (==) `on` midpoint

instance Show a => Show (Interval a) where
    showsPrec n (I (Round a) (Round b)) =   
        showParen (n > 3) $
            showsPrec 3 a .
            showString " ... " . 
            showsPrec 3 b

-- flip the rounding mode up
u :: Round Down a -> Round Up a
u (Round a) = Round a
{-# INLINE u #-}

-- flip the rounding mode down
d :: Round Up a -> Round Down a
d (Round a) = Round a
{-# INLINE d #-}

-- | Calculate the width of an interval.
-- N.B. the width of an interval is an interval itself due to rounding
width :: Precision a => Interval a -> Interval a
width x@(I a b) = I (d b - a) (b - u a)
{-# INLINE width #-}

-- | magnitude 
magnitude :: Precision a => Interval a -> a 
magnitude x = (max `on` abs) (inf x) (sup x)

-- | "mignitude"
mignitude :: Precision a => Interval a -> a 
mignitude x = (min `on` abs) (inf x) (sup x)

instance Precision a => Num (Interval a) where
    I a b + I a' b' = I (a + a') (b + b')
    I a b - I a' b' = I (a - d b') (b - u a')
    I a b * I a' b' = minimum [a * a',a * d b',d b * a',d b * d b'] 
                      `I` 
                      maximum [u a * u a',u a * b',b * u a',b * b']
    abs x@(I a b) 
        | a >= 0    = x 
        | b <= 0    = negate x
        | otherwise = max (- a) (d b) `I` b

    signum (I a b)  = signum a `I` signum b

    fromInteger i   = fromInteger i `I` fromInteger i

-- | Bisect an interval at its midpoint.
bisection :: Precision a => Interval a -> (Interval a, Interval a)
bisection (I a b) = (I a (u a + (b - u a) / 2), I (a + (d b - a) / 2) b)
{-# INLINE bisection #-}

-- | Nearest point to the midpoint of the interval.
midpoint :: Precision a => Interval a -> a
midpoint x = inf x + (sup x - inf x) / 2
{-# INLINE midpoint #-}


elem :: Precision a => a -> Interval a -> Bool
elem x xs = x >= inf xs && x <= sup xs
{-# INLINE elem #-}

notElem :: Precision a => a -> Interval a -> Bool
notElem x xs = not (elem x xs)
{-# INLINE notElem #-}

-- | This means that realToFrac will use the midpoint
instance Precision a => Real (Interval a) where
    toRational x = toRational (midpoint x)

-- @'divNonZero' X Y@ assumes @0 `'notElem'` Y@
divNonZero :: Precision a => Interval a -> Interval a -> Interval a 
divNonZero (I a b) (I a' b') = 
    minimum [a / a',a / d b',d b / a',d b / d b'] 
    `I`
    maximum [u a / u a',u a / b',b / u a',b / b']

-- @'divPositive' X y@ assumes y > 0, and divides @X@ by [0 ... y]
divPositive :: Precision a => Interval a -> a -> Interval a 
divPositive x@(I a b) y
    | a == 0 && b == 0 = x
    | b < 0 || isNegativeZero b = negInfinity `I` ( b / up y)
    | a < 0 = whole 
 -- | isNegativeZero a = whole
    | otherwise = (a / down y) `I` posInfinity

-- divNegative assumes y < 0 and divides the interval @X@ by [y ... 0]
divNegative :: Precision a => Interval a -> a -> Interval a
divNegative x@(I a b) y
    | a == 0 && b == 0 = - x -- flips negative zeros
    | b < 0 || isNegativeZero b = (d b / down y) `I` posInfinity
    | a < 0 = whole
 -- | isNegativeZero a = whole
    | otherwise = negInfinity `I` (u a / up y)

divZero :: Precision a => Interval a -> Interval a
divZero x | inf x == 0 && sup x == 0 = x
          | otherwise = whole

instance Precision a => Fractional (Interval a) where
    -- TODO: check isNegativeZero properly
    x@(I a b) / y@(I a' b')
        | 0 `notElem` y = divNonZero x y 
        | iz && sz  = empty -- division by 0
        | iz        = divPositive x (inf y)
        |       sz  = divNegative x (sup y)
        | otherwise = divZero x
        where 
            iz = inf y == 0
            sz = sup y == 0
    recip (I a b)   = on min recip a (d b) `I` on max recip (u a) b
    fromRational r  = fromRational r `I` fromRational r

instance Precision a => RealFrac (Interval a) where
    properFraction x = (b, x - fromIntegral b)
        where 
            b = truncate (midpoint x)
    ceiling x = ceiling (sup x)
    floor x = floor (inf x)
    round x = round (midpoint x)
    truncate x = truncate (midpoint x)

instance Precision a => Floating (Interval a) where
    pi = pi `I` pi 
    exp (I a b) = exp a `I` exp b
    log (I a b) = (if a > 0 then log a else -1/0) `I` log b
    cos x 
        | inf (width t) >= inf pi = (-1) ... 1
        | u tl >= pih  = - cos (t - pi)
        | d th <= pil  = cos (d th) `I` cos (u tl)
        | d th <= pi2l = (-1) `I` cos (u (min (pi2l - d th) tl))
        | otherwise  = (-1) ... 1
        where 
            I pil pih = pi
            pi2@(I pi2l pi2h) = pi * 2
            t@(I tl th) = x `fmod` pi2
            l = inf t
            h = sup t
    sin x = cos (x - pi / 2)
    
        

-- | We have to play some semantic games to make these methods make sense.
-- Most compute with the midpoint of the interval.
instance Precision a => RealFloat (Interval a) where
    floatRadix = floatRadix . midpoint
    floatDigits = floatDigits . midpoint
    floatRange = floatRange . midpoint
    decodeFloat = decodeFloat . midpoint
    encodeFloat m e = singleton (encodeFloat m e)
    exponent = exponent . midpoint
    significand x = min a b ... max a b
        where
            (mm,em) = decodeFloat (midpoint x)
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
    atan2 = error "unimplemented"

-- TODO: (^), (^^) to give tighter bounds
        
-- | Calculate the intersection of two intervals.
intersection :: Precision a => Interval a -> Interval a -> Interval a
intersection x@(I a b) y@(I a' b')
    | x /=! y = empty
    | otherwise = I (max a a') (min b b')

-- | Calculate the convex hull of two intervals
hull :: Ord a => Interval a -> Interval a -> Interval a
hull x@(I a b) y@(I a' b') 
    | null x = y
    | null y = x
    | otherwise = I (min a a') (max b b')
    
instance Precision a => RealExtras (Interval a) where
    type C (Interval a) = C a
    -- output always lies within the interval y if y >=! 0
    fmod x y | null y = empty 
             | inf y >= 0 = r -- `intersection` bounds
        where 
            n = floor (inf x / if inf x < 0 then inf y else sup y)
            r = x - fromIntegral n * y 
            bounds | inf y >= 0 = y
                   | otherwise = y `hull` negate y
    
    
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

-- | Comparisons are made on the midpoint
instance Precision a => Ord (Interval a) where
    compare = compare `on` midpoint
    max (I a b) (I a' b') = I (max a a') (max b b')
    min (I a b) (I a' b') = I (min a a') (min b b')

ambiguous :: String -> a
ambiguous s = error $ s ++ ": ambiguous result"

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


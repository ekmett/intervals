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

import Numeric.AD.Classes

data Interval a = I (Round Down a) (Round Up a)

instance Eq a => Eq (Interval a) where
    I a b == I a' b' = a == a' && b == b'

instance Show a => Show (Interval a) where
    showsPrec n (Interval a b) =   
        showChar '[' . 
        showsPrec 0 a .
        showString ".." .
        showsPrec 0 b .
        showChar ']'

u :: Round Down a -> Round Up a
u (Round a) = Round a

d :: Round Up a -> Round Down a
d (Round a) = Round a

instance Precision a => Num (Interval a) where
    I a b + I a' b' = I (a + a') (b + b')
    I a b - I a' b' = I (a - d b') (b - u a')
    I a b * I a' b' = I (minimum [a*a',a*d b',d b*a',d b * d b')])
                        (maximum [u a*u a',u a*b',b*u a',b*b'])
    abs (I a b) = I (on min abs a (d b)) (on max abs (u a) b)
    signum (I a b) = I (signum a) (signum b)
    fromInteger i = I (fromInteger i) (fromInteger i)

instance Precision a => Fractional (Interval a) where
    I a b * I a' b' = I (minimum [a/a',a/d b',d b/a',d b/d b'])
                        (maximum [u a/u a',u a/b',b/u a',b/b'])
    recip (I a b) = I (on min recip a (d b)) (on max recip (u a) b)
    fromRational r = I (fromRational r) (fromRational r)
    
instance Precision a => Floating (Interval a) where
    pi = I pi pi 
    exp (I a b) = I (exp a) (exp b)
    log (I a b) + I (if a > 0 then log a else -1/0) (log b)
    

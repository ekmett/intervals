

module Numeric.Interval.Boolean 
  ( differences
  , intersections
  , unions
  ) where

import Numeric.Interval.Internal


-- code below adapted from `range-set-list`

-- | Remove @(,y]@ while (re-)adding @(y,v]@ if valid
trim' :: Ord a => a -> a -> [Interval a] -> [Interval a]
trim' y v xs
  | y < v = I y v : xs
  | otherwise = trim y xs

-- | Remove @(,y]@
trim :: Ord a => a -> [Interval a] -> [Interval a]
trim y     (Empty : xs) = trim y xs
trim y set@(I u v : xs)
  | y < u = set
  | otherwise = trim' y v xs
trim _ [] = []

-- | Add @[x,y]@ to the beginning (assuming @x <= u@).
prepend :: Ord a => a -> a -> [Interval a] -> [Interval a]
prepend x y     (Empty : xs) = prepend x y xs
prepend x y set@(I u v : xs)
  | y < u     = I x y : set
  | otherwise = prepend x (max y v) xs
prepend x y [] = [I x y]

-- | Compute the set difference, removing each range in the second list from
-- the first.
-- >>> differences [0 ... 10] [15 ... 20]
-- [0 ... 10]
-- >>> differences [0 ... 10] [5 ... 10]
-- [0 ... 5]
-- >>> let a = [0...1,6...7,8...10,13...15,16...19]
-- >>> let b = [2...3,4...5,9...11,12...14,17...18]
-- >>> differences a b
-- [0...1,6...7,8...9,14...15,16...17,18...19]
differences :: Ord a => [Interval a] -> [Interval a] -> [Interval a]
differences aset                      (Empty : bs) = differences aset bs
differences      (Empty : as)    bset              = differences as bset
differences aset@(xy@(I x y):as) bset@(I u v : bs)
  | y < u = xy : differences as bset
  | v < x = differences aset bs
  | x < u = I x u : t
  | otherwise = t where
  t = differences (trim' v y as) bs
differences s [] = s
differences [] _ = []


-- | Compute the intersection.
-- >>> intersections [0 ... 5] [10 ... 20]
-- []
-- >>> intersections [0 ... 10] [10 ... 20]
-- [10 ... 10]
-- >>> let a = [0...1,6...7,8...10,13...15,16...19]
-- >>> let b = [2...3,4...5,9...11,12...14,17...18]
-- >>> intersections a b
-- [9...10,13...14,17...18]
intersections :: Ord a => [Interval a] -> [Interval a] -> [Interval a]
intersections aset                   (Empty : bs) = differences aset bs
intersections      (Empty : as) bset              = differences as bset
intersections aset@(I x y : as) bset@(I u v : bs)
  | y < u = intersections as bset
  | v < x = intersections aset bs
  | y < v = I (max x u) y : intersections as bset
  | otherwise = I (max x u) v : intersections aset bs
intersections _ [] = []
intersections [] _ = []

-- | Union two range lists.
-- >>> unions [0 ... 10] [10 ... 20]
-- [0 ... 20]
-- >>> unions [0 ... 5] [10 ... 20]
-- [0 ... 5,10 ... 20]
-- >>> let a = [0...1,6...7,8...10,13...15,16...19]
-- >>> let b = [2...3,4...5,9...11,12...14,17...18]
-- >>> unions a b
-- [0...1,2...3,4...5,6...7,8...11,12...15,16...19]
unions :: Ord a => [Interval a] -> [Interval a] -> [Interval a]
unions aset                   (Empty : bs) = differences aset bs
unions      (Empty : as) bset              = differences as bset
unions aset@(xy@(I x y):as) bset@(uv@(I u v):bs)
  | y < u = xy : unions as bset
  | v < x = uv : unions aset bs
  | otherwise = prepend (min x u) (max y v) $ unions as bs
unions s [] = s
unions [] s = s

difference :: Ord a => Interval a -> Interval a -> [Interval a]
difference a b = differences [a] [b]

intersection :: Ord a => Interval a -> Interval a -> [Interval a]
intersection a b = intersections [a] [b]

union :: Ord a => Interval a -> Interval a -> [Interval a]
union a b = unions [a] [b]

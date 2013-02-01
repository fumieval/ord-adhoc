{-|
Module      :  Data.Ord.Bounded
Copyright   :  (C) 2013 Fumiaki Kinoshita
License     :  BSD-style (see the file LICENSE)
Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>

Creating bounded value from any Ord instance
-}
module Data.Ord.Bounded (
    GBounded
    ,BoundedMin
    ,BoundedMax
    ,BoundedBoth
    ,minimumBound
    ,maximumBound
    ) where

import Control.Applicative
import Data.Void

-- | A structure that provides minimum/maximum to any value.
data GBounded min max a = MinimumB min | ValueB a | MaximumB max deriving (Show, Eq)

type BoundedMin = GBounded () Void
type BoundedMax = GBounded Void ()
type BoundedBoth = GBounded () ()

-- | Provided minimum value.
minimumBound :: GBounded () max a
minimumBound = MinimumB ()

-- | Provided maximum value.
maximumBound :: GBounded min () a
maximumBound = MaximumB ()

instance Functor (GBounded min max) where
    fmap f (ValueB a) = ValueB (f a)
    fmap _ (MinimumB v) = MinimumB v
    fmap _ (MaximumB v) = MaximumB v

instance Applicative (GBounded min max) where
    pure = ValueB
    ValueB f <*> ValueB x = ValueB (f x)
    MinimumB v <*> _ = MinimumB v
    MaximumB v <*> _ = MaximumB v

instance Monad (GBounded min max) where
    return = ValueB
    ValueB x >>= k = k x
    MinimumB v >>= _ = MinimumB v
    MaximumB v >>= _ = MaximumB v

instance (Ord min, Ord max, Ord a) => Ord (GBounded min max a) where
    ValueB a `compare` ValueB b = compare a b
    MinimumB v `compare` MinimumB w = compare v w
    MaximumB v `compare` MaximumB w = compare v w
    MinimumB _ `compare` _ = LT
    _ `compare` MinimumB _ = GT
    MaximumB _ `compare` _ = GT
    _ `compare` MaximumB _ = LT
    
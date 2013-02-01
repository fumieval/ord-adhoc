{-|
Module      :  Data.Ord.Ala
Copyright   :  (C) 2013 Fumiaki Kinoshita
License     :  BSD-style (see the file LICENSE)
Maintainer  :  Fumiaki Kinsohita <fumiexcel@gmail.com>

Creating Ord instance by a key function
-}
module Data.Ord.Ala (Ala(..), ordBy, unAla, keyAla, mapAla) where

-- | Also known as store comonad
newtype Ala k a = Ala (a -> k, a)

instance Eq k => Eq (Ala k a) where
    a == b = keyAla a == keyAla b

instance Ord k => Ord (Ala k a) where
    a `compare` b = keyAla a `compare` keyAla b

-- | Make 'Ord' instance from the key function.
ordBy :: Ord k => (a -> k) -> a -> Ala k a
ordBy f x = Ala (f, x)

-- | Extract the original value from 'Ala'.
unAla :: Ala k a -> a
unAla (Ala (_, x)) = x

-- | Extract the key from 'Ala'.
keyAla :: Ala k a -> k
keyAla (Ala (f, x)) = f x

mapAla :: (j -> k) -> Ala j a -> Ala k a
mapAla f (Ala (k, a)) = Ala (f . k, a)

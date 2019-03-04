{-# LANGUAGE InstanceSigs #-}
module Block4 (buildPair, buildNonEmpty) where

-- Задание 1.

data Pair a = Pair a a deriving (Show)

instance Foldable Pair where
    foldMap :: Monoid b => (a -> b) -> Pair a -> b
    foldMap f (Pair first second) = (f first) `mappend` (f second)

    foldr :: (a -> b -> b) -> b -> Pair a -> b
    foldr f zero (Pair first second) = f first $ f second zero

buildPair :: a -> a -> Pair a
buildPair x y = (Pair x y)

data NonEmpty a = a :| [a] deriving (Show)

instance Foldable NonEmpty where
    foldMap :: Monoid b => (a -> b) -> NonEmpty a -> b
    foldMap f (x :| xs) = (f x) `mappend` (foldMapHelper f xs) where
        foldMapHelper :: Monoid b => (a -> b) -> [a] -> b
        foldMapHelper _ [] = mempty
        foldMapHelper g (y:ys) = foldMap g (y :| ys)

    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f zero (x :| xs) = f x $ foldrHelper f zero xs where
        foldrHelper :: (a -> b -> b) -> b -> [a] -> b
        foldrHelper _ z [] = z
        foldrHelper g z (y:ys) = foldr g z (y :| ys)

buildNonEmpty :: a -> [a] -> NonEmpty a
buildNonEmpty x xs = x :| xs


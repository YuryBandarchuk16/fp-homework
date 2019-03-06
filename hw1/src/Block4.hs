{-# LANGUAGE InstanceSigs #-}
module Block4
    (
      Pair(..)
    , NonEmpty(..)
    , buildPair
    , mySplitOn
    , myJoinWith
    ) where

-- Задание 1.

data Pair a = Pair a a deriving (Show)

instance Foldable Pair where
    foldMap :: Monoid b => (a -> b) -> Pair a -> b
    foldMap f (Pair first second) = (f first) `mappend` (f second)

    foldr :: (a -> b -> b) -> b -> Pair a -> b
    foldr f zero (Pair first second) = f first $ f second zero

buildPair :: a -> a -> Pair a
buildPair x y = (Pair x y)

data NonEmpty a = a :| [a] deriving (Eq, Show)

instance Foldable NonEmpty where
    foldMap :: Monoid b => (a -> b) -> NonEmpty a -> b
    foldMap f (x :| xs) = (f x) `mappend` (foldMapHelper f xs) where
        foldMapHelper :: Monoid b => (a -> b) -> [a] -> b
        foldMapHelper _ []     = mempty
        foldMapHelper g (y:ys) = foldMap g (y :| ys)

    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f zero (x :| xs) = f x $ foldrHelper f zero xs where
        foldrHelper :: (a -> b -> b) -> b -> [a] -> b
        foldrHelper _ z []     = z
        foldrHelper g z (y:ys) = foldr g z (y :| ys)

instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    (<>) x y = foldr makePrepend y x where
        makePrepend :: a -> NonEmpty a -> NonEmpty a
        makePrepend v (z :| zs) = (v :| (z:zs))

toUsualList :: NonEmpty a -> [a]
toUsualList (x :| xs) = x:xs

toNonEmpty :: [a] -> NonEmpty a
toNonEmpty []     = error "can not create non empty, the given list is empty"
toNonEmpty (x:xs) = x :| xs

-- Задание 2.

mySplitOn :: Char -> String -> NonEmpty String
mySplitOn sep s = let uncleanedResult = foldr splitAcc ("" :| []) s in
                        cleanResult uncleanedResult where
                                    splitAcc :: Char -> NonEmpty String -> NonEmpty String
                                    splitAcc c (x :| xs) = if c == sep then ("" :| (x:xs)) else ((c:x) :| xs)
                                    cleanResult :: NonEmpty String -> NonEmpty String
                                    cleanResult r = toNonEmpty $ filter (/= "") (toUsualList r)

-- Задание 2. Усложенная версия.

myJoinWith :: Char -> NonEmpty String -> String
myJoinWith sep parts = foldr joinHelper "" parts where
                        joinHelper :: String -> String -> String
                        joinHelper newString acc = if (length acc) == 0 then newString else newString ++ [sep] ++ acc

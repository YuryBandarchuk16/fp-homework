{-# LANGUAGE InstanceSigs #-}
module Block5
    (
      maybeConcat
    , eitherConcat
    , semigroupWorksForNonEmpty
    , semigroupWorksForThisOrThat
    , fromString
    , toString
    , toStringWithFromStringMakeId
    , ThisOrThat(..)
    , Builder(..)
    , Name(..)
    ) where

import Block4 (NonEmpty (..))

-- Задание 1.

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat elements = mconcat (map unwrapMaybeList elements) where
    unwrapMaybeList :: Maybe [a] -> [a]
    unwrapMaybeList Nothing   = mempty
    unwrapMaybeList (Just xs) = xs

-- Задание 1. Усложненная версия.

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat elements = let (lefts, rights)  = foldr extractor ([], []) elements in
                            (mconcat lefts, mconcat rights) where
                                extractor :: Either a b -> ([a], [b]) -> ([a], [b])
                                extractor (Left x)  (ls, rs) = ((x:ls), rs)
                                extractor (Right x) (ls, rs) = (ls, (x:rs))

-- Задание 2.
-- Инстанс Semigroup для NonEmpty был реализован в Block4, поэтому здесь просто примерчик

semigroupWorksForNonEmpty :: Bool
semigroupWorksForNonEmpty =
    (1 :| ([2, 3] :: [Int])) <> ((4 :| ([5, 6] :: [Int])) <> (7 :| ([8, 9] :: [Int]))) ==
    ((1 :| ([2, 3] :: [Int])) <> (4 :| ([5, 6] :: [Int]))) <> (7 :| ([8, 9] :: [Int]))

data ThisOrThat a b = This a | That b | Both a b deriving (Eq, Show)

instance Semigroup (ThisOrThat a b) where
    (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
    (<>) (This x)   (This _)   = (This x)
    (<>) (This x)   (That y)   = (Both x y)
    (<>) (That x)   (That _)   = (That x)
    (<>) (That x)   (This y)   = (Both y x)
    (<>) (Both x y) _          = (Both x y)
    (<>) (This x)   (Both _ z) = (Both x z)
    (<>) (That x)   (Both y _) = (Both y x)

semigroupWorksForThisOrThat :: Bool
semigroupWorksForThisOrThat = let x = This 10 :: ThisOrThat Int String in
                                let y = That "hi" :: ThisOrThat Int String in
                                    let z = Both 30 "bye" :: ThisOrThat Int String in
                                        x <> (y <> z) == (x <> y) <> z

-- Задание 2. Усложненная версия.

data Name = Name String deriving (Show)

instance Semigroup Name where
    (<>) :: Name -> Name -> Name
    (<>) (Name "") n       = n
    (<>) n (Name "")       = n
    (<>) (Name a) (Name b) = Name (a ++ "." ++ b)

instance Monoid Name where
    mempty :: Name
    mempty = Name ""

-- Задание 3.

data Builder = One Char | Many [Builder] deriving (Show)

instance Semigroup Builder where
    (<>) :: Builder -> Builder -> Builder
    (<>) a b = Many [a, b]

instance Monoid Builder where
    mempty :: Builder
    mempty = Many []

fromString :: String -> Builder
fromString ""     = mempty
fromString (c:cs) = (One c) `mappend` (fromString cs)

toString :: Builder -> String
toString (Many [])       = ""
toString (One c)         = [c]
toString (Many builders) = mconcat $ map toString builders

toStringWithFromStringMakeId :: Bool
toStringWithFromStringMakeId = let testString = "some random test string" in
                                    (myId testString) == testString where
                                        myId :: String -> String
                                        myId = toString . fromString

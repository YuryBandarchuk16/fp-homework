{-# LANGUAGE InstanceSigs #-}
module Block1 
  (
    stringSum
  , Tree(..)
  , NonEmpty(..)
  , testTreeFunctorLaws
  , testNonEmptyFunctorLaws
  , testTreeApplicativeLaws
  , testTreeFoldable
  , testNonEmptyApplicativeLaws
  , testNonEmptyMonadLaws
  , testNonEmptyFoldable
  ) where

import Text.Read(readMaybe)
    
-- Задание 1.

stringSum :: String -> Maybe Int
stringSum s = sum <$> maybeInts where
  maybeInts :: Maybe [Int]
  maybeInts = traverse (((+) <$> (Just 0)) <*>) $ wordsToMaybeInts $ words s
  wordsToMaybeInts :: [String] -> [Maybe Int]
  wordsToMaybeInts = map (readMaybe :: String -> Maybe Int)

-- Задание 2.

data Tree a = Branch (Tree a) (Tree a) | Leaf a deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

testTreeFunctorLaws :: Bool
testTreeFunctorLaws =
  let someTree :: Tree Int
      someTree = Branch (Branch (Leaf 10) (Leaf 20)) (Leaf 1)
      testFirstLaw :: Bool
      testFirstLaw = ((fmap id someTree) == someTree)
      testSecondLaw :: Bool
      testSecondLaw =
        let f = (*2)
            g = (*3) in
              ((fmap (f . g) someTree) == (fmap f . fmap g) someTree) in
        testFirstLaw && testSecondLaw

instance Applicative Tree where
  pure :: a -> Tree a
  pure x = Leaf x

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f) t = f <$> t
  (<*>) (Branch fleft fright) (Branch left right)
    = (Branch (fleft <*> left) (fright <*> right))
  (<*>) (Branch fleft fright) leaf = Branch (fleft <*> leaf) (fright <*> leaf)

testTreeApplicativeLaws :: Bool
testTreeApplicativeLaws =
  let globalLaw :: Bool
      globalLaw = 
        let someTree :: Tree Int
            someTree = Branch (Branch (Leaf 10) (Leaf 20)) (Leaf 1)
            applicativeTree = pure (*5) in
              ((fmap (*5) someTree) == (applicativeTree <*> someTree))
      identityLaw :: Bool
      identityLaw = ((pure id <*> simpleTree) == simpleTree) where
        simpleTree :: Tree Int
        simpleTree = Leaf 10
      compositionLaw :: Bool
      compositionLaw =
        let u' :: Tree Int
            u' = Branch (Leaf 10) (Branch (Leaf 20) (Leaf 30))
            u :: Tree (Int -> Int)
            u = (*) <$> u'
            v' :: Tree Int
            v' = Branch (Branch (Branch (Leaf 10) (Leaf 20)) (Leaf 30)) (Leaf 40)
            v :: Tree (Int -> Int)
            v = (+) <$> v'
            w :: Tree Int
            w = Branch (Leaf 10) (Leaf 20) in
              ((pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w)))
      homomorphismLaw :: Bool
      homomorphismLaw = 
        let someFunction :: Int -> Int
            someFunction = (*2)
            someX :: Int
            someX = 100500
            pureX :: Tree Int
            pureX = pure someX
            leftPart :: Tree Int
            leftPart = pure someFunction <*> pureX
            rightPart :: Tree Int
            rightPart = pure (someFunction someX) in
              (leftPart == rightPart)
      interchangeLaw :: Bool
      interchangeLaw =
        let u' :: Tree Int
            u' = Branch (Leaf 10) (Branch (Leaf 20) (Leaf 30))
            u :: Tree (Int -> Int)
            u = (*) <$> u'
            y :: Int
            y = 200600 in
              ((u <*> pure y) == (pure ($ y) <*> u)) in
        globalLaw      && 
        identityLaw    && 
        compositionLaw &&
        interchangeLaw &&
        homomorphismLaw 

instance Foldable Tree where
  foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Branch left right) = (foldMap f left) `mappend` (foldMap f right)

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f zero (Leaf x) = f x zero
  foldr f zero (Branch left right) = foldr f (foldr f zero right) left

testTreeFoldable :: Bool
testTreeFoldable = 
  let simpleTree :: Tree Int
      simpleTree = Branch (Branch (Leaf 10) (Leaf 20)) (Leaf 30)
      test1 = (foldr (*) 1 simpleTree) == 6000
      test2 = (foldr (:) [] simpleTree) == [10, 20, 30] in
        test1 && test2
    
instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Branch left right) = Branch <$> (traverse f left) <*> (traverse f right)

-- Задание 3.

data NonEmpty a = a :| [a] deriving (Eq, Show)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = (f x) :| (fmap f xs)

testNonEmptyFunctorLaws :: Bool
testNonEmptyFunctorLaws =
  let someList :: NonEmpty Int
      someList = 10 :| [1, 2, 3]
      testFirstLaw :: Bool
      testFirstLaw = ((fmap id someList) == someList)
      testSecondLaw :: Bool
      testSecondLaw =
        let f = (+8)
            g = (*10) in
              ((fmap (f . g) someList) == (fmap f . fmap g) someList) in
        testFirstLaw && testSecondLaw
        
instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| []) l = f <$> l
  (<*>) (f :| xs) (l :| ls) = (f l) :| ((f <$> ls) ++ (xs <*> (l:ls)))
  
testNonEmptyApplicativeLaws :: Bool
testNonEmptyApplicativeLaws =
  let globalLaw :: Bool
      globalLaw = 
        let someNonEmpty :: NonEmpty Int
            someNonEmpty = 10 :| [20, 30, 40, 50]
            applicativeNonEmpty = pure (*5) in
              (((*5) <$> someNonEmpty) == (applicativeNonEmpty <*> someNonEmpty))
      identityLaw :: Bool
      identityLaw = ((pure id <*> simpleNonEmpty) == simpleNonEmpty) where
        simpleNonEmpty :: NonEmpty Int
        simpleNonEmpty = 10 :| []
      compositionLaw :: Bool
      compositionLaw =
        let u' :: NonEmpty Int
            u' = 17 :| [7, 8, 19, 10, 25, 12]
            u :: NonEmpty (Int -> Int)
            u = (*) <$> u'
            v' :: NonEmpty Int
            v' = 10 :| [20, 30, 40]
            v :: NonEmpty (Int -> Int)
            v = (+) <$> v'
            w :: NonEmpty Int
            w = 10 :| [20, 30] in
              ((pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w)))
      homomorphismLaw :: Bool
      homomorphismLaw = 
        let someFunction :: Int -> Int
            someFunction = (*2)
            someX :: Int
            someX = 100500
            pureX :: NonEmpty Int
            pureX = pure someX
            leftPart :: NonEmpty Int
            leftPart = pure someFunction <*> pureX
            rightPart :: NonEmpty Int
            rightPart = pure (someFunction someX) in
              (leftPart == rightPart)
      interchangeLaw :: Bool
      interchangeLaw =
        let u' :: NonEmpty Int
            u' = 10 :| [20, 30, 40, 50]
            u :: NonEmpty (Int -> Int)
            u = (*) <$> u'
            y :: Int
            y = 200600 in
              ((u <*> pure y) == (pure ($ y) <*> u)) in
        globalLaw      && 
        identityLaw    && 
        compositionLaw &&
        interchangeLaw &&
        homomorphismLaw

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

testNonEmptyFoldable :: Bool
testNonEmptyFoldable = 
  let simpleNonEmpty :: NonEmpty Int
      simpleNonEmpty = 10 :| [20, 30]
      test1 = (foldr (+) 0 simpleNonEmpty) == 60
      test2 = (foldr (:) [] simpleNonEmpty) == [10, 20, 30] in
        test1 && test2    
 
instance Traversable NonEmpty where
  traverse :: (Applicative f) => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = (:|) <$> (f x) <*> (traverse f xs)
  
instance Monad NonEmpty where
  return :: a -> NonEmpty a
  return x = x :| []
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) l f = flatten (traverse f l) where
    flatten :: NonEmpty (NonEmpty c) -> NonEmpty c
    flatten (c :| cs) = foldr concatNonEmpty c cs where
      concatNonEmpty :: NonEmpty c -> NonEmpty c -> NonEmpty c
      concatNonEmpty (x:|xs) (y:|ys) = (x:|(xs ++ (y:ys))) 

testNonEmptyMonadLaws :: Bool
testNonEmptyMonadLaws =
  let testFirstLaw :: Bool
      testFirstLaw = 
        let someInt :: Int
            someInt = 100500
            f :: Int -> NonEmpty (Maybe Int)
            f x = (Just x) :| [] in
            ((return someInt >>= f) == (f someInt))
      testSecondLaw :: Bool
      testSecondLaw =
        let someList :: NonEmpty Int
            someList = 1 :| [2, 3] in
            ((someList >>= return) == someList)
      testThirdLaw :: Bool
      testThirdLaw =
        let someList :: NonEmpty Int
            someList = 10 :| [20, 30]
            f :: Int -> NonEmpty (Maybe Int)
            f x = (Just x) :| []
            g :: Maybe Int -> NonEmpty (Maybe Int)
            g (Just x) = (Just (x + 1)) :| []
            g (Nothing) = Nothing :| [] in
            (((someList >>= f) >>= g) == (someList >>= (\x -> f x >>= g))) in
        testFirstLaw && testSecondLaw && testThirdLaw

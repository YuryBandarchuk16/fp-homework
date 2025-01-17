{-# LANGUAGE InstanceSigs #-}
module Block3
    (
      nextDay
    , afterDays
    , isWeekend
    , daysToParty
    , plus
    , minus
    , mult
    , natToInt
    , intToNat
    , equal
    , isLess
    , isLessOrEqual
    , isGreater
    , isGreaterOrEqual
    , isEven
    , divide
    , remainder
    , buildCastle
    , buildChurch
    , buildLibrary
    , buildHouseForFamily
    , moveLordInDaCastle
    , buildWalls
    , isEmptyTree
    , countElementsInTree
    , containsElementInTree
    , insertElementIntoTree
    , buildTreeFromList
    , removeElementFromTree
    , Day(..)
    , EitherLord(..)
    , EitherWalls(..)
    , HasCastle(..)
    , EitherCastle(..)
    , EitherChurchOrLibrary(..)
    , LivingFamily(..)
    , OneHouse(..)
    , LivingHouses(..)
    , City(..)
    , Tree(..)
    ) where

import Data.List.NonEmpty as NonEmpty

-- Задание 1.

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show)

nextDay :: Day -> Day
nextDay day = case day of
    Monday    -> Tuesday
    Tuesday   -> Wednesday
    Wednesday -> Thursday
    Thursday  -> Friday
    Friday    -> Saturday
    Saturday  -> Sunday
    Sunday    -> Monday

afterDays :: Day -> Int -> Day
afterDays day numberOfDays = (foldl (.) id $ (replicate (numberOfDays `mod` 7) nextDay)) day

isWeekend :: Day -> Bool
isWeekend day = case day of
    Saturday -> True
    Sunday   -> True
    _        -> False

daysToParty :: Day -> Int
daysToParty day = Prelude.head (Prelude.filter willBeFriday [0..6])
    where
    isFriday Friday = True
    isFriday _      = False
    willBeFriday numDays = isFriday (afterDays day numDays)


-- Задание 2.

data EitherLord = Lord | NoLord
data EitherWalls = Walls | NoWalls
data HasCastle = Castle EitherLord EitherWalls
data EitherCastle = BuiltCastle HasCastle | NoCastle
data EitherChurchOrLibrary = Church | Library | NoChurchNeitherLibrary
data LivingFamily = OneMemberFamily | TwoMembersFamily | ThreeMembersFamily | FourMembersFamily
data OneHouse = House LivingFamily
data LivingHouses = SingleHouse OneHouse | ManyHouses LivingHouses OneHouse

data City = Megapolis EitherCastle EitherChurchOrLibrary LivingHouses

defaultCastle :: HasCastle
defaultCastle = (Castle NoLord NoWalls)

buildCastle :: City -> (City, Bool)
buildCastle (Megapolis NoCastle eitherCL livingHouses) =
    ((Megapolis (BuiltCastle defaultCastle) eitherCL livingHouses), True)
buildCastle city = (city, False)

buildChurch :: City -> (City, Bool)
buildChurch (Megapolis eitherCastle NoChurchNeitherLibrary livingHouses) =
    ((Megapolis eitherCastle Church livingHouses), True)
buildChurch city = (city, False)

buildLibrary :: City -> (City, Bool)
buildLibrary (Megapolis eitherCastle NoChurchNeitherLibrary livingHouses) =
    ((Megapolis eitherCastle Library livingHouses), True)
buildLibrary city = (city, False)

buildHouseForFamily :: City -> LivingFamily -> City
buildHouseForFamily (Megapolis eitherCastle eitherCL livingHouses) family =
    let newLivingHouses = (extendLivingHouses livingHouses (House family)) in
        (Megapolis eitherCastle eitherCL newLivingHouses) where
            extendLivingHouses :: LivingHouses -> OneHouse -> LivingHouses
            extendLivingHouses houses newHouse = ManyHouses houses newHouse

moveLordInDaCastle :: City -> City
moveLordInDaCastle (Megapolis eitherCastle eitherCL livingHouses) =
    (Megapolis (moveLordHelper eitherCastle) eitherCL livingHouses) where
        moveLordHelper :: EitherCastle -> EitherCastle
        moveLordHelper NoCastle                                  = error "Lord can not move in the castle because the city does not have a castle"
        moveLordHelper (BuiltCastle (Castle Lord _))             = error "Lord can not move in the castle because there is already a lord in the castle"
        moveLordHelper (BuiltCastle (Castle NoLord eitherWalls)) = BuiltCastle (Castle Lord eitherWalls)

getFamilySize :: LivingFamily -> Int
getFamilySize OneMemberFamily    = 1
getFamilySize TwoMembersFamily   = 2
getFamilySize ThreeMembersFamily = 3
getFamilySize FourMembersFamily  = 4

countNumberOfPeopleInHouses :: LivingHouses -> Int
countNumberOfPeopleInHouses (SingleHouse (House family)) = getFamilySize family
countNumberOfPeopleInHouses (ManyHouses livingHouses (House family)) = (getFamilySize family) + countNumberOfPeopleInHouses livingHouses

buildWalls :: City -> City
buildWalls (Megapolis (BuiltCastle (Castle Lord NoWalls)) eitherCL livingHouses) =
    if countNumberOfPeopleInHouses livingHouses >= 10 then (Megapolis (BuiltCastle (Castle Lord Walls)) eitherCL livingHouses)
    else error "Not enough people to build the walls. Needed at least 10 people."
buildWalls (Megapolis (BuiltCastle (Castle Lord Walls)) _ _) = error "The city has walls already built"
buildWalls (Megapolis (BuiltCastle (Castle NoLord _)) _ _) = error "The city must have a lord for walls to be built"
buildWalls (Megapolis NoCastle _ _) = error "The city must have a castle for walls to be built"

-- Задание 3.

data Nat = Z | S Nat

plus :: Nat -> Nat -> Nat
plus Z x     = x
plus x Z     = x
plus (S x) y = plus x (S y)

minus :: Nat -> Nat -> Nat
minus x Z         = x
minus Z _         = Z
minus (S x) (S y) = minus x y

mult :: Nat -> Nat -> Nat
mult _ Z     = Z
mult Z _     = Z
mult (S x) y = plus y (mult x y)

natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S x) = 1 + natToInt x

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat x = S $ intToNat $ x - 1

equal :: Nat -> Nat -> Bool
equal a b = bothZero (minus a b) (minus b a) where
                bothZero :: Nat -> Nat -> Bool
                bothZero Z Z = True
                bothZero _ _ = False

isLess :: Nat -> Nat -> Bool
isLess a b = greaterThanZero $ minus b a where
                greaterThanZero :: Nat -> Bool
                greaterThanZero Z = False
                greaterThanZero _ = True

isLessOrEqual :: Nat -> Nat -> Bool
isLessOrEqual a b = if (a `equal` b) then True else isLess a b

isGreater :: Nat -> Nat -> Bool
isGreater a b = isLess b a

isGreaterOrEqual :: Nat -> Nat -> Bool
isGreaterOrEqual a b = isLessOrEqual b a

-- Задание 3. Усложненная часть

isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S Z)) = True
isEven (S (S x)) = isEven x

divide :: Nat -> Nat -> Nat
divide _ Z = error "Division by zero"
divide a b = calculateSubtractions a b where
              calculateSubtractions :: Nat -> Nat -> Nat
              calculateSubtractions from what =
                if (isLess from what) then Z
                else S (calculateSubtractions (from `minus` what) what)

remainder :: Nat -> Nat -> Nat
remainder _ Z = error "Division by zero"
remainder a b = a `minus` ((a `divide` b) `mult` b)

-- Задание 4.

data Tree a = Leaf | Node (NonEmpty a) (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
    show Leaf = "[Leaf]"
    show (Node elements left right) = (show left) ++ ", " ++ (show elements) ++ ", " ++ (show right)

isEmptyTree :: Tree a -> Bool
isEmptyTree Leaf = True
isEmptyTree _    = False

countElementsInTree :: Tree a -> Int
countElementsInTree Leaf = 0
countElementsInTree (Node elements left right) = (NonEmpty.length elements) + (countElementsInTree left) + (countElementsInTree right)

containsElementInTree :: Ord a => Tree a -> a -> Bool
containsElementInTree Leaf _ = False
containsElementInTree (Node elements left right) target =
    let currentX = NonEmpty.head elements in
        if target == currentX then True
        else if target > currentX then containsElementInTree right target
        else containsElementInTree left target

insertElementIntoTree :: Ord a => Tree a -> a -> Tree a
insertElementIntoTree Leaf x = (Node (fromList [x]) Leaf Leaf)
insertElementIntoTree (Node elements left right) x =
    let currentX = NonEmpty.head elements in
        if currentX == x then (Node (x :| (toList elements)) left right)
        else if x > currentX then (Node elements left (insertElementIntoTree right x))
        else (Node elements (insertElementIntoTree left x) right)

buildTreeFromList :: Ord a => [a] -> Tree a
buildTreeFromList []     = Leaf
buildTreeFromList (x:xs) = insertElementIntoTree (buildTreeFromList xs) x

removeElementFromTree :: Ord a => Tree a -> a -> Tree a
removeElementFromTree Leaf _ = Leaf
removeElementFromTree (Node elements left right) x =
    let currentX = NonEmpty.head elements in
        if currentX == x then removeHelper elements left right
        else if x > currentX then (Node elements left (removeElementFromTree right x))
        else (Node elements (removeElementFromTree left x) right) where
            removeHelper :: NonEmpty a -> Tree a -> Tree a -> Tree a
            removeHelper (_:|[]) leftNode Leaf  = leftNode
            removeHelper (_:|[]) Leaf rightNode = rightNode
            removeHelper (_:|[]) leftNode rightNode =
                let (maximalElementsInLeft, leftWithoutMaxNode) = getAndRemoveMaxNode leftNode in
                    (Node (fromList maximalElementsInLeft) leftWithoutMaxNode rightNode) where
                        getAndRemoveMaxNode :: Tree a -> ([a], Tree a)
                        getAndRemoveMaxNode Leaf = error "can not get max node from empty tree"
                        getAndRemoveMaxNode (Node curElements _ Leaf) = ((toList curElements), Leaf)
                        getAndRemoveMaxNode (Node curElements l r) =
                            let (maxElements, restoredTree) = getAndRemoveMaxNode r in
                                (maxElements, (Node curElements l restoredTree))
            removeHelper (_:|xs) leftNode rightNode = (Node (fromList xs) leftNode rightNode)

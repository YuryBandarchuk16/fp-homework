module Task5
    (
      zero
    , succChurch
    , churchPlus
    , churchMult
    , churchToInt
    , testChurchWorks
    ) where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero _ x = x

succChurch :: Nat a -> Nat a
succChurch num f x = num f (f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b f x = a f (b f x)

churchMult :: Nat a -> Nat a -> Nat a
churchMult a b f x = (a . b) f x

churchToInt :: Nat Integer -> Integer
churchToInt num = num (+1) 0

testChurchWorks :: Bool
testChurchWorks = let one = succChurch zero
                      two = succChurch one
                      three = succChurch two
                      five = churchPlus two three
                      six = churchMult two three
                      intFive = churchToInt five                 -- Проверяет, что 2 + 3 (черчевские) = 5
                      intSix = churchToInt six                   -- Проверяет, что 2 * 3 (черчевские) = 6
                      intZero = churchToInt zero                 -- Проверяет, что 0 (черчевский) = 0
                      intNext = churchToInt (succChurch six) in  -- Проверяет, что следующий за 6 (черчевской) = 7
                        (intFive == 5) && (intSix == 6) && (intZero == 0) && (intNext == 7)

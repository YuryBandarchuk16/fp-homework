module Block1
    (
      order3
    , smartReplicate
    , contains
    , stringSum
    ) where

-- Задание 1.

order3' :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
order3' (a, b, c) =
    let first = min (min a b) c
        lastest = max (max a b) c in
            (first, a + b + c - first - lastest, lastest)

order3 :: (Int, Int, Int) -> (Int, Int, Int)
order3 (a, b, c) =
    let (ra, rb, rc) = order3' (toInteger a, toInteger b, toInteger c) in
        (fromInteger ra, fromInteger rb, fromInteger rc)

-- Задание 2.

smartReplicate :: [Int] -> [Int]
smartReplicate []     = []
smartReplicate (x:xs) = (replicate x x) ++ (smartReplicate xs)

-- Задание 3.

contains :: Int -> [[Int]] -> [[Int]]
contains target lists = filter (elem target) lists

-- Задание 4.

stringSum :: String -> Int
stringSum s = sum (map read $ words s :: [Int])

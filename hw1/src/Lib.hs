module Lib
       (
        order3,
        smartReplicate,
        contains,
        stringSum,
        removeFromListByIndex,
        mergeSort
       ) where

--- | БЛОК 1 | ---

-- Задание 1.
-- Упорядочить по возрастанию переданную тройку элементов.

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
-- Функция должна повторять каждый элемент столько раз, чему равен сам элемент.

smartReplicate :: [Int] -> [Int]
smartReplicate [] = []
smartReplicate (x:xs) = (replicate x x) ++ (smartReplicate xs)

-- Задание 3.
-- Напишите функцию, которой передаётся список списков и некоторый элемент.
-- Эта функция должна вернуть список только тех списков, которые содержат переданный элемент.

contains :: Int -> [[Int]] -> [[Int]]
contains target lists = filter (elem target) lists

-- Задание 4.
-- Требуется написать функцию, которая принимает строку,
-- состоящую только из чисел, разделённых пробельными символами, и находит сумму всех чисел в этой строке.
-- Тип функции должен быть String -> Int. В данном задании нужно считать, что переданная строка корректная.

stringSum :: String -> Int
stringSum s = sum (map read $ words s :: [Int])

--- | БЛОК 2 | ---

-- Задание 1.
-- Реализовать функцию, которая удаляет элемент по заданному индексу (будем использовать 0-индексацию)
--  и возвращает удалённый элемент. 
-- Подумайте над тем, как отразить такое поведение в типе функции наиболее полно.

-- Если только подумать, то в идеале нужно было бы написать что-то в стиле:
-- removeFromListByIndex :: List n Int -> Fin n -> List (n - 1) Int

removeFromListByIndex :: [Int] -> Int -> Maybe [Int]
removeFromListByIndex xs index = 
    if length xs <= index then Nothing
    else Just $ (slice 0 (index - 1) xs) ++ (slice (index + 1) (length xs - 1) xs)

slice :: Int -> Int -> [Int] -> [Int]
slice from to xs = take (to - from + 1) (drop from xs)

-- Задание 2.
-- Требуется реализовать сортировку слиянием.

merge :: [Int] -> [Int] -> [Int]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) = 
    if x < y then x:(merge xs (y:ys)) 
    else y:(merge (x:xs) ys)

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    let middle = (length xs) `div` 2
        left = mergeSort $ take middle xs
        right = mergeSort $ drop middle xs in
            merge left right
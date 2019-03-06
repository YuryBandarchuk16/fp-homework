module Task4
    (
      fix
    , iterateElement
    , fibonacci
    , factorial
    , mapFix
    ) where

import Data.Function (fix)

iterateElement :: a -> [a]
iterateElement x = fix (\l -> (x:l))

fibonacci :: Integer -> Integer
fibonacci = fix fibonacciHelper where
    fibonacciHelper :: (Integer -> Integer) -> Integer -> Integer
    fibonacciHelper f n = 
        if n < 0 then error "fibonacci is not defined for negative numbers"
        else if n <= 1 then n 
        else f (n - 1) + f (n - 2)

factorial :: Integer -> Integer
factorial = fix factorialHelper where
    factorialHelper :: (Integer -> Integer) -> Integer -> Integer
    factorialHelper f n = 
        if n < 0 then f (-n) * ((-1) ^ (n `mod` 2))
        else if n <= 1 then 1
        else n * f (n - 1)

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix mapHelper where
    -- mapHelper :: (a -> b) -> [a] -> [b]
    mapHelper _ _ [] = []
    mapHelper f mapFunc (x:xs) = (mapFunc x):(f mapFunc xs)
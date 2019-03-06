module Task4
    (
      fix
    , iterateElement
    ) where

import Data.Function (fix)

iterateElement :: a -> [a]
iterateElement x = fix (\l -> (x:l))

-- fibonacci :: Integer -> Integer
-- fibonacci n = fix fibonacciHelper (-1) 1 n where
--     fibonacciHelper _ cur next 0 = cur
--     fibonacciHelper f cur next n =
--         let newCur = next
--             newNext = cur + next in
--                  f newCur newNext

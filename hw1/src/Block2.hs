module Block2
    (
      removeFromListByIndex
    , mergeSort
    ) where

-- Задание 1.

removeFromListByIndex :: [Int] -> Int -> Maybe [Int]
removeFromListByIndex xs index =
    if length xs <= index || index < 0 then Nothing
    else Just $ (slice 0 (index - 1) xs) ++ (slice (index + 1) (length xs - 1) xs)

slice :: Int -> Int -> [Int] -> [Int]
slice from to xs = take (to - from + 1) (drop from xs)

merge :: [Int] -> [Int] -> [Int]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) =
    if x < y then x:(merge xs (y:ys))
    else y:(merge (x:xs) ys)

-- Задание 2.

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
    let middle = (length xs) `div` 2
        left = mergeSort $ take middle xs
        right = mergeSort $ drop middle xs in
            merge left right

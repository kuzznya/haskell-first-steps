module Sort 
    (   
        quicksort,
        mergesort
    ) where

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort (filter (<= x) xs) ++ [x] ++ quicksort (filter (> x) xs)

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =
    let
        firstHalf = take (length xs `div` 2) xs
        secondHalf = drop (length xs `div` 2) xs
        merge [] [] = []
        merge xs [] = xs
        merge [] ys = ys
        merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys) else y : merge (x:xs) ys
    in
        merge (mergesort firstHalf) (mergesort secondHalf)

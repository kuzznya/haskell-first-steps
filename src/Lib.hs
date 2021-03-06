module Lib (
    fizzBuzz,
    printFizzBuzz,
    genMatrix,
    quadFunc,
    reverseList,
    runApp
) where

import System.Random
import Data.Char
import qualified Data.Map as Map

import Sort
import Geometry

fizzBuzz = [
    show x ++
    if x `mod` 15 == 0 then " FIZZBUZZ"
    else if x `mod` 3 == 0 then " FIZZ"
    else if x `mod` 5 == 0 then " BUZZ"
    else ""
    | x <- [1..] ]

printFizzBuzz :: Int -> IO ()
printFizzBuzz n = putStrLn (foldl1 (\acc x -> acc ++ "\n" ++ x) (take n fizzBuzz))

printFizzBuzz' n = putStrLn $ unlines (take n fizzBuzz)

genMatrix :: Random r => Int -> (r,r) -> IO [[r]]
genMatrix n range = do
    randomValues <- randomList range
    let matrixValues = take (n ^ 2) randomValues
    return $ map (\i -> take n (drop (i * n) matrixValues)) [0..n-1]

randomList :: Random r => (r, r) -> IO [r]
randomList range = getStdGen >>= \gen -> return (randomRs range gen)

quadFunc :: Num a => [[a]] -> [a] -> a
quadFunc matrix xs = sum (map (uncurry sumLine) (enumerate matrix))
    where
        sumLine i line = sum (map (uncurry (elementValue i)) (enumerate line))
        elementValue i j val = val * xs!!i * xs!!j
        enumerate = zip [0..]

reverseList :: [a] -> [a]
reverseList [] = []
reverseList [x] = [x]
reverseList (x:xs) = reverseList xs ++ [x]

strToInt :: String -> Maybe Int
strToInt [] = Nothing
strToInt cs = Just $ convertReversed $ reverse cs
    where
        convertReversed :: String -> Int
        convertReversed [c] = digitToInt c
        convertReversed (c:cs) = digitToInt c + convertReversed cs * 10

binarySearch val xs = search 0 (length xs)
    where
        meanIdx from to = (to + from) `div` 2
        meanValue from to = xs !! meanIdx from to
        search from to
            | from == to = from
            | otherwise  = compareAndSearch from to (meanValue from to)
        compareAndSearch from to meanVal
            | meanVal > val = search from (to `div` 2)
            | meanVal < val = search (from `div` 2 + 1) to
            | otherwise     = meanIdx from to

printArea :: (Area shape) => shape -> IO ()
printArea shape = print $ area shape

runApp :: IO ()
runApp = do
    printFizzBuzz 15
    matrix <- genMatrix 3 (0 :: Int, 10)
    print matrix
    print (quadFunc matrix [1,2,3])
    print $ reverseList [1,2,3]
    print $ quicksort [3, 7, 4, 6, 1]
    print $ mergesort [3, 7, 4, 6, 1]
    l <- randomList (0 :: Int, 100)
    print $ quicksort (take 1000 l)
    print $ mergesort (take 1000 l)
    print $ binarySearch 1 [0, 2, 3, 1, 5, 6]
    putStrLn "Enter number that should be doubled"
    str <- getLine
    print $ case strToInt str of
        Just val -> val * 2
        Nothing -> error "strToInt error"
    let map = Map.fromList [("kuzznya", "Ilya Kuznetsov"), ("afterbvrner", "Max Golish")]
    print map
    let circle = Circle (Vector 0 1) 10
    print circle
    printArea circle
    let rectangle = Rectangle (Vector 10 10) (Vector 0 0)
    print rectangle
    printArea rectangle

module Lib
    (
        fizzBuzz,
        printFizzBuzz,
        genMatrix,
        quadFunc,
        reverseList,
        runApp
    ) where

import System.Random
import Data.Char

import Sort

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
    putStrLn "Enter number that should be doubled"
    str <- getLine 
    print $ case strToInt str of 
        Just val -> val * 2
        Nothing -> error "strToInt error"

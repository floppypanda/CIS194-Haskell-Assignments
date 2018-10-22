module Golf where

import Data.List

-- Pass in list, starting position, and number to skip.
skip :: [a] -> Int -> Int -> [a]
skip [] _ _ = []
skip (elem:otherElems) position numSkip
    | (position `mod` numSkip) == 0 = elem : skip otherElems (position + 1) numSkip
    | otherwise = skip otherElems (position + 1) numSkip

-- Returns all skips for a given list.
skips :: [a] -> [[a]]
skips [] = []
skips list = map (skip list 1) [1..(length list)]

localMaxima :: [Int] -> [Int]
localMaxima [] = []
localMaxima (x1:[]) = []
localMaxima (x1:x2:[]) = []
localMaxima (x1:x2:x3:xs)
  | x1 < x2 && x2 > x3 = x2:(localMaxima (x2:x3:xs))
  | otherwise = localMaxima (x2:x3:xs)

needAsterix :: Int -> Int -> String
needAsterix level num
    | num >= level = "*"
    | otherwise = " "

buildHistogramLine :: [Int] -> Int -> String
buildHistogramLine [] _ = ""
buildHistogramLine frequencies level = foldr (++) "" (map (needAsterix level) frequencies)

buildHistogram :: [Int] -> String
buildHistogram frequencies = do
    let maxFrequency = maximum frequencies
    intercalate "\n" (map (buildHistogramLine frequencies) [maxFrequency,(maxFrequency-1)..1]) ++ "\n"

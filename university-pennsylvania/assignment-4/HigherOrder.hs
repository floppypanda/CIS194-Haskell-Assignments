module HigherOrder where

fun1 :: [Integer] -> Integer
fun1 =  foldr (*) 1 . map (subtract 2) . filter (\x -> x `mod` 2 == 0)

getNextElement :: Integer -> Integer
getNextElement n
    | even n = n `div` 2
    | otherwise = 3 * n + 1 

-- The original definition for the fun2 function in the assignment appears to compute
-- the sum of a Collatz sequence. Assuming that the Collatz Conjecture is true, we 
-- should take elements from the infinite sequence until 1 is reached.
collatzSequenceSum :: Integer -> Integer
collatzSequenceSum 1 = 1
collatzSequenceSum n = 1 + n + sum (takeWhile (/=1)  (iterate getNextElement n))

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

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

makeNode :: Integer -> a -> Tree a
makeNode num elem = Node num Leaf elem Leaf

makeNodes :: [a] -> [Tree a]
makeNodes list = zipWith ($) (map makeNode $ map toInteger [0..(length list)-1]) list

combineNodes :: Tree a -> Tree a -> Tree a
combineNodes Leaf Leaf = Leaf
combineNodes Leaf tree = tree
combineNodes tree Leaf = tree
combineNodes (Node num Leaf elem Leaf) tree = Node num tree elem Leaf
combineNodes (Node num leftTree elem Leaf) tree = Node num leftTree elem tree
combineNodes (Node num Leaf elem rightTree) tree = Node num tree elem rightTree
combineNodes (Node num leftTree elem rightTree) tree = (Node num (combineNodes leftTree tree) elem rightTree)

foldTree :: [a] -> Tree a
foldTree = foldl combineNodes Leaf . makeNodes

xor :: [Bool] -> Bool
xor = foldl (\a b -> (not a && b) || (a && not b)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y) []

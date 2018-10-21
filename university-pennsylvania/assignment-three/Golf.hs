module Golf where

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

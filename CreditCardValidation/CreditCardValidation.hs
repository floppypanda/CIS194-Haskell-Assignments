-- Returns the length of a list of integers.
len :: [Int] -> Int
len [] = 0
len (x:xs) = 1 + len(xs)

-- Determines if an integer is even.
isEven :: Int -> Bool
isEven n = (n `mod` 2 == 0)

-- Converts an integer to a list of digits.
toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [(n `mod` 10)]

-- Adds the digits of an integer.
addDigits :: Int -> Int
addDigits 0 = 0
addDigits n = (n `mod` 10) + addDigits(n `div` 10)

-- Doubles every second digit in a list of integers.
doubleEveryOtherDigit :: [Int] -> Bool -> [Int]
doubleEveryOtherDigit ([]) (n) = []
doubleEveryOtherDigit (x:[]) (n) = [x]
doubleEveryOtherDigit (x:y:zs) (True) = (x * 2) : y : (doubleEveryOtherDigit zs True)
doubleEveryOtherDigit (x:y:zs) (False) = x : (y * 2) : (doubleEveryOtherDigit zs False)

-- Add digits of integer list.
addListDigits :: [Int] -> Int
addListDigits [] = 0
addListDigits (x:xs) = addDigits(x) + addListDigits(xs)

-- Determines if a card number is valid.
isValidCardNumber :: Int -> Bool
isValidCardNumber n = ((addListDigits (doubleEveryOtherDigit (toDigits n) (isEven (len (toDigits n))))) `mod` 10) == 0

main :: IO ()
--main = putStr $ show $ verifyCreditCard 4012888888881882
main = putStr $ show $ hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

-- Hanoi
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c = []

-- Credit card validation
verifyCreditCard :: Integer -> Bool
verifyCreditCard n = sumDigits (doubleFromLast $ toDigits n) `mod` 10 == 0

firstDigit :: Integer -> Integer
firstDigit n = n `mod` 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
   | n <= 0 = []
   | otherwise = firstDigit n : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

sumDigits :: [Integer] -> Integer
sumDigits = foldr (\ x -> (+) (sum $ toDigits x)) 0

everyOther :: [Integer] -> (Integer -> Integer) -> [Integer]
everyOther [] fn = []
everyOther [x] fn = [fn x]
everyOther (x:y:xs) fn = fn x : y : everyOther xs fn

doubleFromLast :: [Integer] -> [Integer]
doubleFromLast xs = everyOther xs (* 2)

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fib where

fib :: Integer -> Integer
fib n
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)
--fibs2 = map fst $ iterate (\(a,b) -> (b, a + b)) (0,1)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
   show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Cons x y) = Cons (fn x) (streamMap fn y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn x = Cons x (streamFromSeed fn (fn x))

interleaveStreams :: Stream a -> Stream a -> Stream a
--interleaveStreams (Cons x y) (Cons a b) = Cons x $ Cons a $ interleaveStreams y b
interleaveStreams (Cons x y) z = Cons x $ interleaveStreams z y

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = foldr interleaveStreams (streamRepeat 1) (map streamRepeat [0..])

-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .

instance Num (Stream Integer) where
   fromInteger i = Cons i (streamRepeat 0)
   negate = streamMap (*(-1))
   (Cons a0 a') + (Cons b0 b') = Cons (a0 + b0) (a' + b')
   (Cons a0 a') * b@(Cons b0 b') = Cons (a0 * b0) (streamMap (*a0) b' + a' * b)

instance Fractional (Stream Integer) where
   (Cons a0 a') / (Cons b0 b') = res
      where res = Cons (a0 `div` b0) (streamMap (`div` b0) (a' - res * b'))

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - (x * x))

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
   (Matrix a11 a12 a21 a22) * (Matrix b11 b12 b21 b22) =
      Matrix ((a11 * b11) + (a12 * b21)) ((a11 * b12) + (a12 * b22))
             ((a21 * b11) + (a22 * b21)) ((a21 * b12) + (a22 * b22))

fibs4 :: Integer -> Integer
fibs4 n
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = r
      where m = Matrix 1 1 1 0
            (Matrix r _ _ _) = m ^ (n - 1)

module Wholemeal where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
   | even x = (x - 2) * fun1 xs
   | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
   | even n = n + fun2 (n `div` 2)
   | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Exercise 2
data Tree a = Leaf
   | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

insertTree :: a -> Tree a -> Tree a
insertTree a Leaf = Node 0 Leaf a Leaf
insertTree a (Node _ l v r) = case (l, r) of
   (Node lh _ _ _, Node rh _ _ _) ->
      if lh >= rh
      then let nn@(Node nh _ _ _) = insertTree a r in Node (nh + 1) l v nn
      else let nn@(Node nh _ _ _) = insertTree a l in Node (nh + 1) nn v r
   (Leaf, _) -> Node 1 (insertTree a l) v r
   (_, Leaf) -> Node 1 l v (insertTree a r)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\ x acc -> if x then not acc else acc) False

-- which returns True if and only if there are an odd number of True
-- values contained in the input list. It does not matter how many
-- False values the input list contains. For example,

-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False

-- Implement map as a fold. That is, complete the definition
map' :: (a -> b) -> [a] -> [b]
--map' f = foldr (\x acc -> f x : acc) []
map' f = foldr ((:) . f) []

-- in such a way that map’ behaves identically to the standard map
-- function

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl fn base xs = foldr (\b -> \g x -> g (fn x b)) id xs base

-- ((((base `fn` x1) `fn` x2)) `fn` x3)
-- ((((base `fn` x3) `fn` x2)) `fn` x1)
--
minus (x:xs) (y:ys) = case compare x y of
    LT -> x : minus  xs (y:ys)
    EQ ->     minus  xs    ys
    GT ->     minus (x:xs) ys
minus  xs  _  = xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = filter odd $ minus [1..(upTo - 1)] [i + j + 2 * i * j | i <- [1..upTo], j <- [1..upTo], i <= j]
   where upTo = (2 * n + 2)

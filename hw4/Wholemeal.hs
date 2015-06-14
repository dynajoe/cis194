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
      then case (insertTree a r) of nn@(Node nh _ _ _) -> (Node (nh + 1) l v nn)
      else case (insertTree a l) of nn@(Node nh _ _ _) -> (Node (nh + 1) nn v r)
   (Leaf, _) -> Node 1 (insertTree a l) v r
   (_, Leaf) -> Node 1 l v (insertTree a r)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

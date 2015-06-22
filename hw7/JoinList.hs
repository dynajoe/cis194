{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
   | Single m a
   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- Write an append function for JoinLists
-- that yields a new JoinList whose monoidal annotation is derived
-- from those of the two arguments.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x `mappend` tag y) x y

-- which gets the annotation at the root of a JoinList.
tag :: Monoid m => JoinList m a -> m
tag (Single x _) = x
tag (Append x _ _) = x
tag _ = mempty

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ j _ | j < 0 = Nothing
indexJ _ Empty = Nothing
indexJ j (Single _ x)
   | j == 0 = Just x
   | otherwise = Nothing
indexJ j (Append x l r)
   | j >= s = Nothing
   | j < ls = indexJ j l
   | otherwise = indexJ (j - ls) r
   where s = getSize $ size x
         ls = getSize $ size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n (Append _ l r)
   | n > sl = dropJ (n - sl) r
   | otherwise = dropJ n l +++ r
   where sl = getSize . size . tag $ l
dropJ n x
   | n >= sa = Empty
   | otherwise = x
   where sa = getSize . size . tag $ x

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n (Append _ b c)
   | n > sb = b +++ takeJ (n - sb) c
   | otherwise = takeJ n b
   where sb = getSize . size . tag $ b
takeJ n x
   | n >= s = x
   | otherwise = Empty
   where s = getSize . size . tag $ x

testList :: JoinList Size String
testList = Append (Size 5)
   (Append (Size 3)
      (Single (Size 1) "A")
      (Append (Size 2)
         (Single (Size 1) "B")
         (Single (Size 1) "C")))
   (Append (Size 2)
      (Single (Size 1) "D")
      (Single (Size 1) "E"))


scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

scoreLine2 :: String -> JoinList (Score, Size) String
scoreLine2 x = Single (scoreString x, Size 1) x

instance Buffer (JoinList (Score, Size) String) where
   toString = unlines . jlToList
   line = indexJ
   fromString = foldr ((+++) . scoreLine2) Empty . lines
   replaceLine n a b = takeJ n b +++ scoreLine2 a +++ dropJ (n + 1) b
   numLines = getSize . size . tag
   value = getScore . fst . tag

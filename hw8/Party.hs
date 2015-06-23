{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree
import Data.List

instance Monoid GuestList where
   mappend (GL e1 f1) (GL e2 f2) = (GL (e1 `mappend` e2) (f1 + f2))
   mempty = (GL mempty 0)

glCons :: Employee -> GuestList -> GuestList
glCons n@(Emp {empFun = x}) (GL e f) = (GL (n : e) (f + x))

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b
   | a >= b = a
   | otherwise = b

-- data Tree a = Node {
   -- rootLabel :: a, -- label value
   -- subForest :: [Tree a] -- zero or more child trees
-- }
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold fn (Node {rootLabel = v, subForest = c}) = fn v (map (treeFold fn) c)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = (bestWithBoss, bestWithoutBoss)
   where withBoss = map (glCons b . fst) gls
         withoutBoss = map snd gls
         bestWithBoss = maximumGL withBoss
         bestWithoutBoss = maximumGL withoutBoss

maximumGL :: [GuestList] -> GuestList
maximumGL [] = mempty
maximumGL x = maximum x

maxFun :: Tree Employee -> GuestList
maxFun n = max (fst r) (snd r)
   where r = treeFold nextLevel n

bestGuestList :: String -> GuestList
bestGuestList = maxFun . read

main = do
   d <- readFile "company.txt"
   let (GL e f) = bestGuestList d
       names = sort $ map empName e in do
         putStrLn $ "Total fun: " ++ show f
         mapM_ putStrLn names
   return ()
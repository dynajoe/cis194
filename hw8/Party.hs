{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree
import Data.List

instance Monoid GuestList where
   mappend (GL e1 f1) (GL e2 f2) = GL (e1 `mappend` e2) (f1 + f2)
   mempty = GL mempty 0

glCons :: Employee -> GuestList -> GuestList
glCons n@(Emp {empFun = x}) (GL e f) = GL (n : e) (f + x)

testaaa :: GuestList -> GuestList -> GuestList
testaaa a gls
   | a >= gls = a
   | otherwise = gls

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold fn (Node {rootLabel = boss, subForest = emps}) = fn boss (map (treeFold fn) emps)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss aaa = (withBoss, withoutBoss)
   where withBoss = glCons boss $ mconcat $ map snd gls
         withoutBoss = mconcat $ map (uncurry max) gls

maximumGL :: [GuestList] -> GuestList
maximumGL [] = mempty
maximumGL x = maximum x

maxFun :: Tree Employee -> GuestList
maxFun employeeTree = uncurry max r
   where r = treeFold nextLevel employeeTree

bestGuestList :: String -> GuestList
bestGuestList = maxFun . read

main :: IO ()
main = do
   d <- readFile "company.txt"
   let (GL e f) = bestGuestList d
       names = sort $ map empName e in do
         putStrLn $ "Total fun: " ++ show f
         mapM_ putStrLn names
   return ()

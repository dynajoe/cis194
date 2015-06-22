{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import StackVM
import ExprT
import Parser (parseExp)
import Data.Maybe
import qualified Data.Map as M

eval :: ExprT -> Integer
eval (ExprT.Add l r) = eval l + eval r
eval (ExprT.Mul l r) = eval l * eval r
eval (ExprT.Lit i) = i

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

class Expr a where
   mul :: a -> a -> a
   add :: a -> a -> a
   lit :: Integer -> a

instance Expr ExprT where
   mul = ExprT.Mul
   add = ExprT.Add
   lit = ExprT.Lit

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
   mul = (*)
   add = (+)
   lit = id

instance Expr Bool where
   mul = (&&)
   add = (||)
   lit = (>0)

newtype MinMax = MinMax Integer
   deriving (Eq, Show)

instance Ord MinMax where
   (MinMax a) <= (MinMax b) = a <= b

instance Expr MinMax where
   mul = min
   add = max
   lit = MinMax

newtype Mod7 = Mod7 Integer
   deriving (Eq, Show)

instance Expr Mod7 where
   mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7
   add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
   lit x = Mod7 (x `mod` 7)

instance Expr Program where
   mul (as) (bs) = as ++ bs ++ [StackVM.Mul]
   add (as) (bs) = as ++ bs ++ [StackVM.Add]
   lit a = [PushI a]

compile :: String -> Maybe Program
compile = parseExp lit add mul

run :: String -> Either String StackVal
-- run s = case compile s of
--    Just p -> stackVM p
run = stackVM . fromMaybe [] . compile

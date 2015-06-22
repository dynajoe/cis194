{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char

newtype Score = Score { getScore :: Int }
   deriving (Eq, Ord, Show, Num)

instance Monoid Score where
   mempty = Score 0
   mappend = (+)

score :: Char -> Score
score x
   | c `elem` ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'S', 'T', 'R'] = Score 1
   | c `elem` ['D', 'G'] = Score 2
   | c `elem` ['B', 'C', 'M', 'P'] = Score 3
   | c `elem` ['F', 'H', 'V', 'W', 'Y'] = Score 4
   | c `elem` ['K'] = Score 5
   | c `elem` ['J', 'X'] = Score 8
   | c `elem` ['Q', 'Z'] = Score 10
   | otherwise = Score 0
   where c = toUpper x

scoreString :: String -> Score
scoreString xs = foldr (+) 0 (map score xs)

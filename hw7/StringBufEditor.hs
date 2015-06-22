module Main where

import JoinList
import Scrabble
import Sized
import Editor

buffer :: JoinList (Score, Size) String
buffer = scoreLine2 ""

main = runEditor editor $ buffer

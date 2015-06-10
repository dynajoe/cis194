module Golf where

skips :: [a] -> [[a]]
skips x = map (\ y -> [v | (v, i) <- zip x r, mod i y == 0]) r
   where r = [1..(length x)]

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:x) = [b | b > a && b > c] ++ localMaxima (b : c : x)
localMaxima _ = []

module Golf where
import Data.List
import Data.Function (on)

skips :: [a] -> [[a]]
skips x = map (\ y -> [v | (v, i) <- zip x r, mod i y == 0]) r
   where r = [1..(length x)]

localMaxima :: [Integer] -> [Integer]
localMaxima (a:r@(b:c:_)) = [b | b > a && b > c] ++ localMaxima r
localMaxima _ = []

count :: Eq a => (a -> Bool) -> [a] -> Int
count fn = length . filter fn

stars :: [Integer] -> [String]
stars xs = concat $ reverse $ map (\r -> map (\c -> if count (==c) xs >= r then "*" else " ") cs ++ ["\n"]) [1..m]
   where a = map (\n -> (count (==n) xs, n)) cs
         m = fromIntegral $ maximum $ map fst a
         cs = [0..9]

histogram :: [Integer] -> String
histogram xs = concat (stars xs ++ ["==========\n0123456789\n"])

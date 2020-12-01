{-# OPTIONS_GHC -Wall #-}

import Text.ParserCombinators.ReadP

readNumbers :: String -> [Int]
readNumbers s = case [x | (x, "") <- readP_to_S numbers s] of
  [x] -> x
  _   -> []

numbers :: ReadP [Int]
numbers = skipSpaces *> endBy1 number skipSpaces

number :: ReadP Int
number = fmap read (munch1 (\c -> c >= '0' && c <= '9'))

pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (_:[]) = []
pairs (x:xs) = map ((,)x) xs ++ pairs xs

triplets :: [a] -> [(a, a, a)]
triplets []       = []
triplets (_:[])   = []
triplets (_:_:[]) = []
triplets (x:xs)   = map (\(a, b) -> (x, a, b)) (pairs xs) ++ triplets xs

solve :: [Int] -> [Int]
solve ns = [a * b | (a, b) <- pairs ns, a + b == 2020]

solve' :: [Int] -> [Int]
solve' ns = [a * b * c | (a, b, c) <- triplets ns, a + b + c == 2020]

main :: IO ()
main = do
  ns <- fmap readNumbers getContents
  putStrLn $ "Part one: " ++ show (solve  ns)
  putStrLn $ "Part two: " ++ show (solve' ns)

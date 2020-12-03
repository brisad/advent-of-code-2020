{-# OPTIONS_GHC -Wall #-}

import Text.ParserCombinators.ReadP
type Map = [[Int]]
type Slope = (Int, Int)

parseWith :: Monoid a => ReadP a -> String -> a
parseWith p s = case [x | (x, "") <- readP_to_S p s] of
  [x] -> x
  _   -> mempty

pmap :: ReadP Map
pmap = endBy row (char '\n')

row :: ReadP [Int]
row = cycle <$> many (open +++ tree)
  where open = const 0 <$> char '.'
        tree = const 1 <$> char '#'

countTrees :: Map -> Slope -> Int
countTrees m (dx, dy) = countTrees' m 0 0
  where countTrees' :: Map -> Int -> Int -> Int
        countTrees' []     _ _= 0
        countTrees' (r:rs) x 0 = r !! x + countTrees' rs (x + dx) (dy - 1)
        countTrees' (_:rs) x y =          countTrees' rs (x + dx) (y - 1)

solve :: Map -> Int
solve m = countTrees m (3, 1)

solve' :: Map -> Int
solve' m = product [countTrees m s | s <- slopes]
  where slopes = [(1,1), (3,1), (5,1), (7,1), (1, 2)]

main :: IO ()
main = do
  m <- fmap (parseWith pmap) getContents
  putStrLn $ "Part one: " ++ show (solve  m)
  putStrLn $ "Part two: " ++ show (solve' m)

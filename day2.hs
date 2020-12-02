{-# OPTIONS_GHC -Wall #-}

import Text.ParserCombinators.ReadP
import Data.Maybe

data Policy = Policy Int Int Char
type Password = String

parseWith :: Monoid a => ReadP a -> String -> a
parseWith p s = case [x | (x, "") <- readP_to_S p s] of
  [x] -> x
  _   -> mempty

entries :: ReadP [(Policy, Password)]
entries = skipSpaces *> endBy1 entry skipSpaces

entry :: ReadP (Policy, Password)
entry = do
  po <- policy
  _  <- string ": "
  pa <- munch1 (\c -> c >= 'a' && c <= 'z')
  return (po, pa)

policy :: ReadP Policy
policy = do
  m <- nat
  _ <- char '-'
  n <- nat
  _ <- char ' '
  c <- get
  return (Policy m n c)

nat :: ReadP Int
nat = fmap read $ munch1 (\c -> c >= '0' && c <= '9')

safeGet :: [a] -> Int -> Maybe a
safeGet s n | n < 1 || n > length s = Nothing
            | otherwise             = Just $ s !! (n - 1)

check :: Policy -> Password -> Bool
check (Policy m n c) password = cnt >= m && cnt <= n
  where cnt = length . filter (== c) $ password

check' :: Policy -> Password -> Bool
check' (Policy m n c) password = fromMaybe False $ pure (/=) <*> at m <*> at n
  where at = fmap (== c) . safeGet password

solve :: [(Policy, Password)] -> Int
solve = length . filter (uncurry check)

solve' :: [(Policy, Password)] -> Int
solve' = length . filter (uncurry check')

main :: IO ()
main = do
  es <- fmap (parseWith entries) getContents
  putStrLn $ "Part one: " ++ show (solve  es)
  putStrLn $ "Part two: " ++ show (solve' es)

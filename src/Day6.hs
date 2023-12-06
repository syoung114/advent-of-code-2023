module Day6 where

import Data.List.Split (splitOn)

dists :: Int -> [Int]
dists t = map (\dt -> (t-dt)*dt) [0..t] -- (t-dt)*dt is the algebraic relationship in the problem. dt is delta time and t is total time.

fn :: [(Int , Int)] -> [Int]
fn xs = map (\(ts,d) -> length $ filter (>d) $ dists ts) xs

fnn :: [Int] -> Int
fnn xs = foldl (*) 1 xs

reformat :: String -> String -> [(Int , Int)]
reformat xs ys = zip (reformat' xs) (reformat' ys)
  where
    reformat' :: String -> [Int]
    reformat' xs = map strToInt (words $ removeGameCount xs)

    strToInt :: String -> Int
    strToInt xs = read xs :: Int

    removeGameCount :: String -> String
    removeGameCount xs = (head . tail . splitOn ":") xs

day6a :: IO [String]
day6a = do
  contents <- readFile "../input/day6.txt"
  let flines = lines contents
      input = reformat (head flines) (last flines)
  return [show $ fnn $ fn input]

--part 2--------------------------------------------------------

reformat2 :: String -> String -> (Int , Int)
reformat2 xs ys = (reformat' xs , reformat' ys)
  where
    reformat' :: String -> Int
    reformat' xs = strToInt (filter (/= ' ') $ removeGameCount xs)

    strToInt :: String -> Int
    strToInt xs = read xs :: Int

    removeGameCount :: String -> String
    removeGameCount xs = (head . tail . splitOn ":") xs

day6b :: IO [String]
day6b = do
  contents <- readFile "../input/day6.txt"
  let flines = lines contents
      input = reformat2 (head flines) (last flines)
  return [show $ fn [input]]

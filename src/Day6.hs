module Day6 where

import Data.List.Split (splitOn)

dists :: Int -> [Int]
dists t = map (\dt -> (t-dt)*dt) [0..t]

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
      dt = reformat (head flines) (last flines)
  return [show $ fnn $ fn dt]

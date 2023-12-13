module Day13 where

import Data.List

hScan :: [String] -> Int
hScan xs = 0

vScan :: [String] -> Int
vScan xs = 0

fn :: [[String]] -> Int
fn xs = foldl (\acc x -> acc + hScan x + 100 * vScan x) 0 xs

reformat :: [String] -> [[String]]
reformat xs = map (\x -> filter (/= "") x) $ groupBy (\x y -> y /= "") xs

day13a :: IO [String]
day13a = do
  content <- readFile "../input/day13.txt"
  let input = lines content
  return [show $ length input]
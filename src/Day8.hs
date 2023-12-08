module Day8 where

import Data.List.Split (splitOn)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Maybe (fromJust)

selectPath :: Char -> (String, String) -> String
selectPath c (a, b)
  | c == 'L' = a
  | c == 'R' = b

pathLength :: String -> String -> (String -> Bool) -> HashMap String (String, String) -> Int
pathLength directions start pred m = driver (cycle directions) start pred 0
  where
    driver :: String -> String -> (String -> Bool) -> Int -> Int
    driver (d:ds) currentNode pred acc
      | pred currentNode = acc
      | otherwise = driver ds (singlePass d currentNode) pred (acc + 1)

    singlePass :: Char -> String -> String
    singlePass d currentNode = selectPath d $ fromJust $ HashMap.lookup currentNode m

reformat :: [String] -> HashMap String (String, String)
reformat xs = reformat' xs HashMap.empty
  where
    reformat' :: [String] -> HashMap String (String, String) -> HashMap String (String, String)
    reformat' [] acc = acc
    reformat' (x:xs) acc = reformat' xs (HashMap.insert k v acc)
      where
        (k, v) = splt x

    splt :: String -> (String, (String, String))
    splt xs =
      let
        whole = splitOn " = " xs
        key = head whole
        value =
          tuple
          $ splitOn ", "
          $ take (length (last whole) - 2) (tail $ last whole)
      in
        (key, value)
      where
        tuple :: [a] -> (a, a)
        tuple [a, b] = (a, b)

day8a :: IO [String]
day8a = do
  contentsRaw <- readFile "../input/day8.txt"
  let flines = lines contentsRaw
      directions = head flines
      input = reformat (drop 2 flines)
  return [show $ pathLength directions "AAA" (\n -> n == "ZZZ") input]

-- Part 2 ---------------------------------------------------------------------

lcmm :: Integral a => [a] -> a
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)

pathLength2 :: String -> HashMap String (String, String) -> Int
pathLength2 directions m =
  lcmm
    $ map
      (\start -> pathLength directions start (\n -> last n == 'Z') m)
      ["AAA", "BBA", "DRA", "PSA", "BLA", "NFA"] -- from input file. I'm too lazy to parse.

day8b :: IO [String]
day8b = do
  contentsRaw <- readFile "../input/day8.txt"
  let flines = lines contentsRaw
      directions = head flines
      input = reformat (drop 2 flines)
  return [show $ pathLength2 directions input]
module Day8 where

import Data.List.Split (splitOn)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Data.Maybe (fromJust)

selectPath :: Char -> (String, String) -> String
selectPath c (a, b)
  | c == 'L' = a
  | c == 'R' = b

pathLength :: String -> HashMap String (String, String) -> Int
pathLength (d:ds) m = pathLength' (cycle (ds++[d])) m (selectPath d $ fromJust $ HashMap.lookup "AAA" m) 1
  where
    pathLength' :: String -> HashMap String (String, String) -> String -> Int -> Int
    pathLength' (d:ds) m currentNode acc
      | currentNode == "ZZZ" = acc -- || nextNode == currentNode = acc
      | otherwise = pathLength' ds m nextNode (acc + 1)
        where
          nextNode = selectPath d $ fromJust $ HashMap.lookup currentNode m

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
  return [show $ pathLength directions input]
module Day14 where

import Data.List
import Data.List.Split
import Data.Tuple.Extra (both)

countFallen :: String -> Int
countFallen xs = 
  foldl
  (
    \acc (i, str) ->
      let
        mStrs = tuple (group str)
      in
        case mStrs of
          Nothing -> acc
          Just strs ->
            let    
              (empties, rocks) = both length strs
            in
              acc + arithmeticSum (empties + i) (empties + rocks + i - 1) (rocks)
  )
  0
  $ zip accLengths sortedParts
  where
    sortedParts = map sort $ splitOn "#" xs
    accLengths = init $ scanl (\acc (a,b) -> acc+a+b) 1 (zip [1..] $ map length sortedParts)
    
    tuple :: [a] -> Maybe (a, a)
    tuple [a, b] = Just (a, b)
    tuple _ = Nothing
  
    arithmeticSum :: Int -> Int -> Int -> Int
    arithmeticSum a1 an n = n * (a1 + an) `div` 2

fn :: [String] -> Int
fn xs = foldl (\acc x -> acc + countFallen x) 0 xs

reformat :: [String] -> [String]
reformat xs = rot90right xs
  where
    transpose :: [[a]] -> [[a]]
    transpose ([]:_) = []
    transpose x = (map head x) : transpose (map tail x)

    rot90right :: [[a]] -> [[a]]
    rot90right xs = map reverse $ transpose xs

day14a :: IO [String]
day14a = do
  content <- readFile "../input/day14.txt"
  let input = reformat $ lines content
  return [show $ fn input]
module Day9 where

import Statistics.LinearRegression
import qualified Data.Vector.Unboxed as U

import Data.Tuple.Extra (both)

grad :: [Int] -> (Int , Int)
grad xs = both round $ mxb 
  where
    uxs = U.fromList [1.. fromIntegral $ length xs]
    uys = U.fromList $ map fromIntegral xs
    mxb = linearRegression uxs uys

splitDifferences :: [Int] -> [Int]
splitDifferences xs = zipWith (-) (tail xs) xs

guessNext :: [Int] -> Int
guessNext xs = guessNext' xs 0
  where
    guessNext' :: [Int] -> Int -> Int
    guessNext' xs acc = 
      let
        (beta, alpha) = grad xs
      in
        if beta == 0 then
          alpha * (length xs + 1) + acc
        else
          guessNext' (splitDifferences xs) (acc + (last xs))

sumNexts :: [[Int]] -> Int
sumNexts xs = foldl (\acc x -> acc + guessNext x) 0 xs

reformat :: [String] -> [[Int]]
reformat xs = map (\x -> map strToInt $ words x) xs
  where
    strToInt :: String -> Int
    strToInt xs = read xs :: Int

day9a :: IO [String]
day9a = do
  contents <- readFile "../input/day9.txt"
  let flines = lines contents
      input = reformat flines
  return [show $ sumNexts input]
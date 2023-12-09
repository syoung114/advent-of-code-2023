module Day9 where

import Statistics.LinearRegression
import qualified Data.Vector.Unboxed as U

guessNext :: [Int] -> Int
guessNext xs = 
  let
    uxs = U.fromList [1.. fromIntegral $ length xs]
    uys = U.fromList $ map fromIntegral xs
    (alpha, beta) = linearRegression uxs uys
  in
    round $ (fromIntegral $ length xs + 1) * (alpha * fromIntegral (length xs) + beta)

sumNexts :: [[Int]] -> Int
sumNexts xs = foldl (\acc x -> acc + guessNext x) 0 xs

-- reformat :: [String] -> [[Int]]
-- reformat xs = map (\x -> map strToInt $ words x) xs
--   where
--     strToInt :: String -> Int
--     strToInt xs = read xs :: Int

day9a :: IO [String]
day9a = do
  contents <- readFile "../input/day9.txt"
  let flines = lines contents
      --input = reformat flines
  return [show $ sumNexts [[1,3,6,10,15,21]]]
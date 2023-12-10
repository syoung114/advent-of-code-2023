module Day9 where

--import Statistics.LinearRegression
--import qualified Data.Vector.Unboxed as U

--import Math.Polynomial.Interpolation

guessNext :: [Int] -> Int
guessNext xs = round $ lagrange (zip [1..] $ map fromIntegral xs) (fromIntegral $ length xs + 1)
  where
    lagrange :: [(Double, Double)] -> Double -> Double
    lagrange xs x = sum $ zipWith (\j (x_,y) -> y * lagrangeBasis xs x (fst $ xs !! j)) [0..] xs

    lagrangeBasis :: [(Double, Double)] -> Double -> Double -> Double
    lagrangeBasis xs x xj = product $ map (\(xm, _) -> if xm == xj then 1 else (x - xm) / (xj - xm)) xs

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
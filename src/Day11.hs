module Day11 where

-- these imports are for part 2
import Data.Tuple.Extra (both)
import Statistics.LinearRegression
import qualified Data.Vector.Unboxed as U

type Coord = (Int, Int)

--nCr :: Int -> Int -> Int
--nCr n r = product [1..n] `div` (product [1..r] * product [1..(n-r)])

formatToCoords :: [String] -> [Coord]
formatToCoords xs = formatToCoords' xs 0 []
  where
    formatToCoords' :: [String] -> Int -> [Coord] -> [Coord]
    formatToCoords' [] _ acc = acc
    formatToCoords' (x:xs) i acc =
      let
        chars = findChars x '#'
      in
        if chars == [] then
          formatToCoords' xs (i+1) acc
        else
          formatToCoords' xs (i+1) 
          (
            acc ++
            map (\j -> (j, i)) chars
          )

    -- findChars 
    findChars :: String -> Char -> [Int]
    findChars xs c = findChars' xs c 0 []

    findChars' :: String -> Char -> Int -> [Int] -> [Int]
    findChars' [] _ _ acc = acc
    findChars' (x:xs) c i acc
      | x == c =
          findChars' xs c (i+1) (acc++[i])
      | otherwise =
          findChars' xs c (i+1) acc

taxicab :: Coord -> Coord -> Int
taxicab (x, y) (i, j) = abs (x - i) + abs (y - j)

distances :: [Coord] -> [Int]
distances xs = [ taxicab x y | x <- xs, y <- xs, x /= y ]

sumDistances :: [Coord] -> Int
sumDistances xs = round $ (fromIntegral $ sum $ distances xs) / 2 -- dividing by 2 removes duplicates

reformat :: Int -> [String] -> [String]
reformat numEmpty xs = rot90left $ doubleEmptySpace numEmpty $ rot90right $ doubleEmptySpace numEmpty xs
  where
    transpose :: [[a]] -> [[a]]
    transpose ([]:_) = []
    transpose x = (map head x) : transpose (map tail x)
    -- source: https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell

    -- the following rotation functions I figured out through Google Images:
    rot90right :: [[a]] -> [[a]]
    rot90right xs = map reverse $ transpose xs

    rot90left :: [[a]] -> [[a]]
    rot90left xs = transpose $ map reverse xs
    
    isEmptySpace :: String -> Bool
    isEmptySpace = all (=='.')

    doubleEmptySpace :: Int -> [String] -> [String]
    doubleEmptySpace numEmpty xs = doubleEmptySpace' numEmpty xs []

    doubleEmptySpace' :: Int -> [String] -> [String] -> [String]
    doubleEmptySpace' _ [] acc = acc
    doubleEmptySpace' numEmpty (x:xs) acc
      | isEmptySpace x =
          doubleEmptySpace' numEmpty xs (acc++(replicate numEmpty x))
      | otherwise =
          doubleEmptySpace' numEmpty xs (acc++[x])

day11a :: IO [String]
day11a = do
  content <- readFile "../input/day11.txt"
  let input = reformat 2 $ lines content
      coords = formatToCoords input
  return [show $ sumDistances coords]

-- part 2 ---------------------------------------------------

plotPoints :: [Int] -> [String] -> [Int]
plotPoints numEmpty xs = map (\e -> sumDistances $ formatToCoords $ reformat e xs) numEmpty

grad :: [Int] -> (Int , Int)
grad xs = both round $ mxb 
  where
    uxs = U.fromList [1.. fromIntegral $ length xs]
    uys = U.fromList $ map fromIntegral xs
    mxb = linearRegression uxs uys

day11b :: IO [String]
day11b = do
  content <- readFile "../input/day11.txt"
  let points = plotPoints [1..10] $ lines content
      (b, m) = grad points -- instead of actually computing a multimillion*multimillion matrix, I just use the gradient from the first 10 sumDistances
  return [show $ m*1000000+b]
module Day11 where

type Coord = (Int, Int)

nCr :: Int -> Int -> Int
nCr n r = product [1..n] `div` (product [1..r] * product [1..(n-r)])

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
taxicab (x, y) (i, j) = abs (x - i) + abs (y - j) --remember to subtract 2

reformat :: [String] -> [String]
reformat xs = rot90left $ doubleEmptySpace $ rot90right $ doubleEmptySpace xs
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

    doubleEmptySpace :: [String] -> [String]
    doubleEmptySpace xs = doubleEmptySpace' xs []

    doubleEmptySpace' :: [String] -> [String] -> [String]
    doubleEmptySpace' [] acc = acc
    doubleEmptySpace' (x:xs) acc
      | isEmptySpace x =
          doubleEmptySpace' xs (acc++[x]++[x])
      | otherwise =
          doubleEmptySpace' xs (acc++[x])

day11a :: IO () 
day11a = do
  content <- readFile "../input/day11.txt"
  let coords = reformat $ lines content
  mapM_ (putStrLn . show) $ coords
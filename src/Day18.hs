module Day18 where

import Data.List.Split
import Data.List (nub)

type Vector = (String, Int)

navigate :: [Vector] -> [(Int, Int)]
navigate vs = scanl (\prev v -> move prev v) (0,0) vs
  where
    move :: (Int, Int) -> Vector -> (Int, Int)
    move (relX,relY) (dir,mag)
      | dir == "R" = (relX+mag,relY)
      | dir == "U" = (relX,relY+mag)
      | dir == "D" = (relX,relY-mag)
      | dir == "L" = (relX-mag,relY)

shoelace :: [(Int,Int)] -> Double
shoelace xs = 0.5 * fromIntegral (abs $ foldlPair(\acc (t1,t2) -> acc + shoelaceBasis t1 t2) 0 xs)
  where
    shoelaceBasis :: (Int,Int) -> (Int,Int) -> Int
    shoelaceBasis (x1,y1) (x2,y2) = (x1*y2) - (x2*y1)

foldlPair :: (b -> (a,a) -> b) -> b -> [a] -> b
foldlPair _ acc [] = acc
foldlPair _ acc [_] = acc -- no more elements because the second element that would normally form the tuple is beyond the end
foldlPair fn acc (x:y:xs) = foldlPair fn (fn acc (x,y)) (y:xs)


scanlPair :: (a -> a -> b) -> [a] -> [b]
scanlPair fn xs = scanlPair' fn [] xs
  where
    scanlPair' :: (a -> a -> b) -> [b] -> [a] -> [b]
    scanlPair' _ acc [] = acc
    scanlPair' _ acc [_] = acc
    scanlPair' fn acc (x:y:xs) = scanlPair' fn (acc ++ [fn x y]) (y:xs)

-- linear interpolation but it's also discrete with differences of 1.
-- ghci> derp (0,0) (6,0)
-- [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(6,0)]
derp :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
derp (x1, y1) (x2, y2)
  | x1 /= x2 = map (\x -> (x, y1)) (derpRange x1 x2)
  | y1 /= y2 = map (\y -> (x1, y)) (derpRange y1 y2)
  where
    derpRange :: Int -> Int -> [Int]
    derpRange a b
      | a <= b = [a..b]
      | otherwise = reverse [b..a]

areaOfPath :: [Vector] -> Int
areaOfPath xs = 
  let fullPath = nub $ init $ concat $ scanlPair derp $ navigate xs
  in
    round $ (0.5 * fromIntegral (length fullPath)) + (shoelace fullPath) + 1.0

reformat :: [String] -> [Vector]
reformat xs = reformat' xs []
  where
    reformat' :: [String] -> [Vector] -> [Vector]
    reformat' [] acc = acc
    reformat' (x:xs) acc = reformat' xs (acc++[i])
      where
        i_ = take 2 $ splitOn " " x
        i = (head i_, strToInt (i_ !! 1))

        strToInt :: String -> Int
        strToInt xs = read xs :: Int

day18a :: IO [String]
day18a = do
  content <- readFile "../input/day18.txt"
  let input = reformat $ lines content
  return [show $ areaOfPath input]

--day18a :: IO ()
--day18a = do
--  content <- readFile "../input/day18.txt"
--  let input = reformat $ lines content
--  mapM_ (putStrLn . show) (navigate input)

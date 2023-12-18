module Day18 where

import Data.List.Split

type Path = [(String, Int, String)]

navigate :: Path -> [(Int, Int)]
navigate ps = navigate' ps []
  where
    navigate' :: Path -> [(Int, Int)] -> [(Int, Int)]
    navigate' [] acc = acc
    navigate' (p:ps) acc = navigate' ps (acc ++ move p x y)
      where
        (x,y) = if length acc > 0 then last acc else (-1,-1)

    move :: (String, Int, String) -> Int -> Int -> [(Int, Int)]
    move ("U",magnitude,_) x y = [ (x_, y_) | y_ <- [(y+1)..(magnitude - y)], x_ <- [x-1] ]
    move ("L",magnitude,_) x y = [ (x_, y_) | x_ <- [(x+1)..(magnitude - x)], y_ <- [y-1] ]
    move ("D",magnitude,_) x y = [ (x_, y_) | y_ <- [(y+1)..(magnitude + y)], x_ <- [x+1] ]
    move ("R",magnitude,_) x y = [ (x_, y_) | x_ <- [(x+1)..(magnitude + x)], y_ <- [y+1] ]

reformat :: [String] -> Path
reformat xs = reformat' xs []
  where
    reformat' :: [String] -> Path -> Path
    reformat' [] acc = acc
    reformat' (x:xs) acc = reformat' xs (acc++[i])
      where
        i_ = splitOn " " x
        i_3 = i_ !! 2
        i = (head i_, strToInt (i_ !! 1), (take (length i_3 - 3) $ drop 2 i_3))

        strToInt :: String -> Int
        strToInt xs = read xs :: Int

day18a :: IO ()
day18a = do
  content <- readFile "../input/day18.txt"
  let input = reformat $ lines content
  mapM_ (putStrLn . show) (navigate input)
--day18a :: IO [String]
--day18a = do
--  content <- readFile "../input/day18.txt"
--  let input = reformat $ lines content
--  return [show $ input]

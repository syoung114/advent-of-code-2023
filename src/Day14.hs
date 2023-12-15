module Day14 where

import Data.List
import Data.List.Split
import Data.Tuple.Extra (both)
import Data.Maybe (mapMaybe)

countFallen :: String -> Int 
countFallen xs = 
  foldl (\acc (_, o) -> acc + o) 0
    $ map (\t -> both accIndices t)
    $ mapMaybe tuple
    $ map (groupBy (\(_, a) (_, b) -> a == '.' && b == '.' || a == 'O' && b == 'O'))
    $ filter (not . any ((=='#') . snd))
    $ groupBy (\(_, a) (_, b) -> a /= '#' && b /= '#')
    $ zip [1..]
      $ concat
      $ map sort
      $ groupBy (\x y -> x /= '#' && y /= '#') xs


accIndices :: [(Int, a)] -> Int
accIndices xs = Data.List.foldl (\acc x -> acc + fst x) 0 xs

tuple :: [a] -> Maybe (a, a)
tuple [x, y] = Just (x, y)
tuple _ = Nothing

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
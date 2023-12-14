module Day13 where

import Data.List
import Data.Array

vScan :: Array (Int , Int) Char -> Int
vScan xs = vScan' xs 0 (fst $ snd $ bounds xs)
  where
    vScan' :: Array (Int , Int) Char -> Int -> Int -> Int
    vScan' xs i end
      | i >= (fst $ snd $ bounds xs) = 0 -- reached end without finding a mirror
      | (rowAt i xs) /= (rowAt end xs) = vScan' xs (i+1) end -- no mirror but keep going
      | otherwise = round $ (fromIntegral (i + end)) / 2.0 -- calculate the relative midpoint

hScan :: Array (Int , Int) Char -> Int
hScan xs = vScan $ rot90right xs

fn :: [Array (Int , Int) Char] -> Int
fn xs = foldl (\acc x -> acc + hScan x + 100 * vScan x) 0 xs

-- boilerplate begins here
rowAt :: Int -> Array (Int , Int) Char -> String
rowAt j xs = [xs ! (j,i) | i <- [0..(fst $ snd $ bounds xs)]]

rot90right :: Array (Int , Int) a -> Array (Int , Int) a
rot90right arr = ixmap (bounds arr) (\(j,i) -> (innerMax - i, j)) arr
  where
    innerMax = fst $ snd $ bounds arr

reformat :: [String] -> [Array (Int , Int) Char]
reformat xs =
  map
  (
    \x ->
      let
        lenInner = (length (head x) - 1)
        lenOuter = length x - 1
        cxx = concat x
      in
        listArray ((0,0),(lenInner,lenOuter)) cxx
  )
  $ map
    (
      \x -> filter (/= "") x
    )
    $ groupBy
      (\x y -> y /= "")
      xs

day13a :: IO [String]
day13a = do
  content <- readFile "../input/day13.txt"
  let input = lines content
  return [show $ length input]
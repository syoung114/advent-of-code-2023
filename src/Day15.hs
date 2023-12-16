module Day15 where

import Data.List.Split (splitOn)

hsh :: String -> Int
hsh xs =
  foldl
    (
      \acc c ->
        (
          (acc + fromEnum c) * 17
        )
        `mod` 256
    )
    0 xs

fn :: [String] -> Int
fn xs = foldl (\acc x -> acc + hsh x) 0 xs

reformat :: String -> [String]
reformat xs = splitOn "," xs

day15a :: IO [String]
day15a = do
  content <- readFile "../input/day15.txt"
  let input = reformat content
  return [show $ fn input]
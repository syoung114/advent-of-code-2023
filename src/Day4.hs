module Day4 where

{-
    We're adding 2**(n-1), for n>0
-}

import Data.Char
import Data.List.Split (splitOn)
import Data.Tuple.Extra

countMatching :: Eq a => ([a] , [a]) -> Int
countMatching (list1,list2) = length $ filter (`elem` list2) list1

pow2Clamp :: Int -> Int
pow2Clamp n
  | n <= 0 = 0
  | otherwise = 2 ^ (n - 1)


tuple :: [a] -> (a, a)
tuple [x, y] = (x, y)

isNumeric :: String -> Bool
isNumeric xs = all isDigit xs && xs /= ""

fn :: [String] -> Int
fn xs = fn' (removeGameCount xs) 0
  where
    removeGameCount :: [String] -> [String]
    removeGameCount xs = map (head . tail . splitOn ": ") xs

    --tuple _ = error "buttcheek on a stick"

    numMatches :: String -> Int
    numMatches x =
      countMatching
        $ both (filter isNumeric) -- clean up things the splitting couldn't do. 
        $ tuple (
            map (splitOn " ") -- [["1", "2", "3"], ["4", ...]]
              $ splitOn " | " x -- split "1 2 3 | 4 5 6" -> ["1 2 3", "4 5 6"]
          )

    fn' :: [String] -> Int -> Int
    fn' [] acc = acc
    fn' (x:xs) acc =
      let 
        matches = numMatches x
      in
        fn' xs (acc + pow2Clamp matches)

day4a :: IO [String]
day4a = do
  input <- readFile "../input/day4.txt"
  let flines = lines input
  return [show $ fn flines]
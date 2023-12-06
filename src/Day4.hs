module Day4 where

{-
    We're adding 2**(n-1), for n>0
-}

import Data.Char
import Data.List.Split (splitOn)
import Data.Tuple.Extra

import Data.Traversable

countMatching :: Eq a => ([a] , [a]) -> Int
countMatching (list1,list2) = length $ filter (`elem` list2) list1

pow2Clamp :: Int -> Int
pow2Clamp n
  | n <= 0 = 0 --prevent rausing to negative power. don't want it in this problem.
  | otherwise = 2 ^ (n - 1)

reformat :: [String] -> [([Int] , [Int])]
reformat xs =
  map
  (
    \x ->
      both (map strToInt . filter isNumeric) -- Clean up things the splitting couldn't do. 
      $ tuple -- Wrap inner map in tuple to avoid explicitly using the same function twice.
        (
          map
          (splitOn " ")
          $ splitOn " | " -- "1 2 3 | 4 5 6" -> [["1", "2", "3"], ["4", "5", "6"]]
            $ (head . tail . splitOn ": ") x -- during computation we want [[]] -> [] so head does that
        )
  )
  xs
  where
    tuple :: [a] -> (a , a)
    tuple [x, y] = (x, y)

    isNumeric :: String -> Bool
    isNumeric xs = all isDigit xs && xs /= ""

    strToInt :: String -> Int
    strToInt xs = read xs :: Int

fn :: [([Int] , [Int])] -> Int
fn xs = foldl (\acc x -> acc + pow2Clamp (countMatching x)) 0 xs

day4a :: IO [String]
day4a = do
  content <- readFile "../input/day4.txt"
  let flines = lines content
      input = reformat flines
  return [show $ fn input]

-- part 2 ---------------------------------------------------

changeRange :: Int -> Int -> (a -> a) -> [a] -> [a]
changeRange i j f xs = take i xs ++ map f (take (j+1-i) $ drop i xs) ++ drop (j+1) xs

cardsWinCards :: [Int] -> Int -> Int -> [Int]
cardsWinCards pointTable length l2 = cardsWinCards' pointTable 0 length (replicate l2 1)
  where
    cardsWinCards' :: [Int] -> Int -> Int -> [Int] -> [Int]
    cardsWinCards' pointTable i length instances
      | i >= length = instances
      -- | pointTable !! i == 0 = instances
      | otherwise =
        let
          currentValidAmount = pointTable !! i
        in
          if currentValidAmount > 0 then
            cardsWinCards' pointTable (i + 1) length $ changeRange (i+1) (i+currentValidAmount) (+currentValidAmount) instances
          else
            --instances
            cardsWinCards' pointTable (i + 1) length instances

pointsPerCard :: [String] -> [Int]
pointsPerCard xs = map countMatching (reformat xs)

fn2 :: [Int] -> Int
fn2 xs = sum (cardsWinCards xs (length xs) (length xs))

day4b :: IO [String]
day4b = do
  content <- readFile "../input/day4.txt"
  let flines = lines content
  return [show $ fn2 $ pointsPerCard flines]

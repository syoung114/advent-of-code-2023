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

numMatches :: String -> Int
numMatches x =
  countMatching
    $ both (filter isNumeric) -- Clean up things the splitting couldn't do. Using tuples to hide using the same function twice. 
    $ tuple (
        map (splitOn " ") -- [["1", "2", "3"], ["4", ...]]
          $ splitOn " | " x -- "1 2 3 | 4 5 6" -> ["1 2 3", "4 5 6"]
      )
    where
      tuple :: [a] -> (a , a)
      tuple [x, y] = (x, y)

      isNumeric :: String -> Bool
      isNumeric xs = all isDigit xs && xs /= ""

removeGameCount :: [String] -> [String]
removeGameCount xs = map (head . tail . splitOn ": ") xs -- during computation we want [[]] -> [] so head does that

fn :: [String] -> Int
fn xs = fn' (removeGameCount xs) 0
  where

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
pointsPerCard xs = map numMatches (removeGameCount xs)

fn2 :: [String] -> Int
fn2 xs = sum (cardsWinCards allPoints numCards numCards)
  where
    allPoints = pointsPerCard xs
    numCards = length allPoints

day4b :: IO [String]
day4b = do
  input <- readFile "../input/day4.txt"
  let flines = lines input
  return [show $ fn2 flines]
module Day2 where

import Data.Char
import Data.List.Split (splitOn)


--Common------------------------

strToInt :: String -> Int
strToInt xs = read xs :: Int

splitAtRange :: Int -> Int -> [a] -> [a]
splitAtRange i j xs = take (j - i + 1) (drop i xs)

findNum :: String -> Int -> Int
findNum xs i = 
  let
    iMin2 = i - 2
    iMin3 = i - 3
  in
    if (iMin3 >= 0) && (isDigit $ xs !! iMin3) then
      strToInt $ splitAtRange iMin3 iMin2 xs
    else
      strToInt $ [xs !! iMin2]



--part 1---------------------------------------------------------


matchColorAmount :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
matchColorAmount xs i (r,g,b)
  | x == 'r' = (r + findNum xs i, g, b)
  | x == 'g' = (r, g + findNum xs i, b)
  | x == 'b' = (r, g, b + findNum xs i)
  | otherwise = (r, g, b)
  where x = xs !! i

countColours :: String -> (Int, Int, Int)
countColours xs = countColours' xs 2 (0,0,0) (length xs) -- start at 2 that's the earliest possible location of a char 'r', 'g' or 'b'
  where
    countColours' :: String -> Int -> (Int, Int, Int) -> Int -> (Int, Int, Int)
    countColours' xs i rgbacc len
      | i >= len = rgbacc
      | otherwise = countColours' xs (i+1) (matchColorAmount xs i rgbacc) len

countAllowed :: (Int, Int, Int) -> Bool
countAllowed (r,g,b) = r <= 12 && g <= 13 && b <= 14

playGame :: [String] -> Bool
playGame [] = True
playGame (x:xs)
  | countAllowed (countColours x) = playGame xs
  | otherwise = False

sumValidGames :: [String] -> Int
sumValidGames flines = sumValidGames' flines 1 0
  where
    sumValidGames' :: [String] -> Int -> Int -> Int
    sumValidGames' [] _ validAcc = validAcc
    sumValidGames' (x:xs) gameCount validAcc = 
      let
        r = splitOn ";" x
      in
        if (playGame r) then
          sumValidGames' xs (gameCount + 1) (gameCount + validAcc)
        else
          sumValidGames' xs (gameCount + 1) validAcc

day2a :: IO [String]
day2a = do
    file <- readFile "input/day2.txt"
    let flines = lines file
    return [show $ sumValidGames flines]


--part 2----------------------------------------------------------

{-
    This one is almost the same except we're traversing until the end of the game and we're looking for the biggest number of r/g/b

    Could refactor so that I am reusing code but not worth it. Common functions are reused.
-}

maxInt :: Int
maxInt = maxBound :: Int

matchColorAmount2 :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
matchColorAmount2 xs i (r,g,b)
  | x == 'r' = (max r $ findNum xs i, g, b)
  | x == 'g' = (r, max g $ findNum xs i, b)
  | x == 'b' = (r, g, max b $ findNum xs i)
  | otherwise = (r, g, b)
  where x = xs !! i

countColours2 :: String -> (Int, Int, Int)
countColours2 xs = countColours2' xs 2 (-1,-1,-1) (length xs) -- start at 2 that's the earliest possible location of a char 'r', 'g' or 'b'
  where
    countColours2' :: String -> Int -> (Int, Int, Int) -> Int -> (Int, Int, Int)
    countColours2' xs i rgbacc len
      | i >= len = rgbacc
      | otherwise = countColours2' xs (i+1) (matchColorAmount2 xs i rgbacc) len

countAllowed2 :: (Int, Int, Int) -> Bool
countAllowed2 (r,g,b) = r <= 12 && g <= 13 && b <= 14

sumValidGames2 :: [String] -> Int
sumValidGames2 flines = sumValidGames2' flines 0
  where
    sumValidGames2' :: [String] -> Int -> Int
    sumValidGames2' [] validAcc = validAcc
    sumValidGames2' (x:xs) validAcc = 
      let
        (r,g,b) = countColours2 x
      in
        sumValidGames2' xs (r*g*b + validAcc)

day2b :: IO [String]
day2b = do
    file <- readFile "input/day2.txt"
    let flines = lines file
    return [show $ sumValidGames2 flines]

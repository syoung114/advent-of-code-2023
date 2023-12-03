module Day2 where

import Data.Char

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

sumValidGames :: [String] -> Int
sumValidGames flines = sumValidGames' flines 1 0
  where
    sumValidGames' :: [String] -> Int -> Int -> Int
    sumValidGames' [] _ validSum = validSum
    sumValidGames' (x:xs) round validSum = 
      let 
        (r,g,b) = countColours x
        isValid = if r <= 12 && g <= 13 && b <= 14 then round else 0
      in
        sumValidGames' xs (round + 1) (isValid + validSum)

day2a :: IO [String]
day2a = do
    file <- readFile "input/day2.txt"
    let flines = lines file
    return [show $ sumValidGames flines]
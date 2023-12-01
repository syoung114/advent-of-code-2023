module Day1 where

import Data.Char

trimStringUntil :: (Char -> Bool) -> String -> String
trimStringUntil _ [] = "" -- base case.
trimStringUntil pred (x:xs)
  | pred x = x:xs -- pred is true. give back the trimmed string and let the caller decide what to with it.
  | otherwise = trimStringUntil pred xs

strToInt :: String -> Int
strToInt xs = read xs :: Int

sumFirstLast :: String -> Int
sumFirstLast xs = 
  let first = trimStringUntil isDigit xs
  in
    if first == "" then 0 -- Short circuit. If first is empty, then there is no last. Therefore sum is 0.
    else
      let
        last = trimStringUntil isDigit (reverse xs)
        z = strToInt ([head first] ++ [head last]) -- "1abcd" and "9zyxw" -> "19"
      in
        z

sumFirstLastLines :: [String] -> Int
sumFirstLastLines xs = sumFirstLastLines' xs 0

sumFirstLastLines' :: [String] -> Int -> Int
sumFirstLastLines' [] z = z
sumFirstLastLines' (x:xs) z = sumFirstLastLines' xs (z + sumFirstLast x) -- tail recursion accumulating the lines.

day1 :: IO [String]
day1 = do
    file <- readFile "day1.txt"
    let flines = lines file
    return [show $ sumFirstLastLines flines]

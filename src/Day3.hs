module Day3 where

{-
    This was solved on modified input data. I did this to solve for a simpler case so that how we approach complexity is more linear.

    My changes include

    2 - All symbols are the same.

    These changes provide a straighforward algorithm where we traverse down the lines, three lines at a time.

    the output remains the same but the algorithm is simpler. Once it's solved, you can easily consider the case where the symbols are different, and 
    handle the first and last lines specially.

    I know that imperatove code could check for characters in a set.

    also note that the input I was given did not have single digits on the starr or end of any line. I can't assure you the same assumption.

-}

import System.IO
import Control.Monad

--sumValidNums :: [String] -> ST s String
--sumValidNums xs = sumValidNums' xs 0
--  where
--    sumValidNums' :: [String] -> ST s String
--    sumValidNums' [] acc = acc
--    sumValidNums' (top:middle:bottom:xs) acc = 


--readFileST :: FilePath -> IO String
--readFileST path = do
--  handle <- openFile path ReadMode
--  contents <- hGetContents handle
--  let singleWords = words contents
--      list = (map read) singleWords
--  print list
--  hClose handle

splitAtRange :: Int -> Int -> [a] -> [a]
splitAtRange i j xs = take (j - i + 1) (drop i xs)

sumKernel :: (ST s String , ST s String , ST s String) -> Int -> Int -> (Int, (ST s String , ST s String , ST s String))
sumKernel (top:middle:bottom) kStart kDead kEnd =
  if middle !! kDead != '#' then do (return 0, (top:middle:bottom))
  else
    -- top !! kDead
    -- top !! kDead + 1
    -- middle !! kDead + 1
    -- bottom !! kDead + 1
    -- bottom !! kDead
    -- bottom !! kDead - 1
    -- middle !! kDead - 1
    -- top !! kDead - 1


  
horizontalSlide :: (ST s String , ST s String , ST s String) -> (Int , (ST s String , ST s String , ST s String))
horizontalSlide (t:mb) = horizontalKernel' (t : mb) (length t) 6 0
  where
    horizontalSlide' :: (ST s String , ST s String , ST s String) -> Int -> Int -> Int -> (Int , (ST s String , ST s String , ST s String))
    horizontalSlide' tmb length i acc
      | i >= length = do (return acc, tmb)
      | otherwise = do
          s <- sumKernel tmb (i - 6) (i - 3) i 
          horizontalSlide' (snd s) length (i + 1) (acc + $ fst s)

verticalSlide :: [ST s String] -> ST s Int
verticalSlide xs = sumKernel' xs
  where
    verticalSlide' :: [ST s String] -> Int -> ST s Int
    verticalSlide' [top, middle] acc = return acc -- stop here because the middle line is the last line. We can't progress further.
    verticalSlide' (top:middle:bottom:xs) acc = do
      newLines = horizontalSlide top middle bottom
      verticalSlide' ((drop 1 $ snd newLines) : xs) (acc + fst newLines)

listToListST :: [a] -> [ST s a]
listToListST = map return -- TODO directly read into ST instead of converting to ST. That removes O(n) (n**2?) steps.

day3a :: IO [String]
day3a = do
  file <- readFile "input/day3.txt"
  let flines = lines file
  return [show $ runST $ do
    STflines <- listToSTlist flines
    result <- verticalSlide STflines
    return result
   ]

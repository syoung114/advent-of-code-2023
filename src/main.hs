module Main where

import System.Environment (getArgs)

import Day1
import Day2

main :: IO ()
main = do
     args <- getArgs
     case args of
         [] -> putStrLn "no args provided"
         ( program : _ ) -> do
             output <- case program of
                 "day1a" -> day1a
                 "day1b" -> day1b
                 "day2a" -> day2a
                 "day2b" -> day2b
                 _ -> return ["invalid choice: " ++ program]
             putStrLn $ unlines output
         --_ -> putStrLn "foobar"

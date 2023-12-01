module Main where

import System.Environment (getArgs)

import Day1

main :: IO ()
main = do
     args <- getArgs
     case args of
         [] -> putStrLn "no args provided"
         ( program : _ ) -> do
             output <- case program of
                 "day1" -> day1
                 _ -> return ["invalid choice: " ++ program]
             putStrLn $ unlines output
         --_ -> putStrLn "foobar"

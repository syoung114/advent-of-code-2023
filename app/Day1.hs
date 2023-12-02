module Day1 where

import Data.Char

-- Part 1 ------------------------------------------------------------

trimStringUntil :: (Char -> Bool) -> String -> String
trimStringUntil _ [] = "" -- base case.
trimStringUntil pred (x:xs)
  | pred x = x:xs -- pred is true. give back the trimmed string and let the caller decide what to with it.
  | otherwise = trimStringUntil pred xs

strToInt :: String -> Int
strToInt xs = read xs :: Int

concatFirstLast :: String -> Int
concatFirstLast xs = 
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
  where
    sumFirstLastLines' :: [String] -> Int -> Int
    sumFirstLastLines' [] acc = acc
    sumFirstLastLines' (x:xs) acc = sumFirstLastLines' xs (acc + concatFirstLast x) -- tail recursion accumulating the lines.

day1a :: IO [String]
day1a = do
    file <- readFile "day1.txt"
    let flines = lines file
    return [show $ sumFirstLastLines flines]

-- part 2 -----------------------------------------------------------

{-

an unbalanced search tree is useful for this approach, similar to

                              root
 _-------_-------_------_------|--------_------------_
one    eight    nine    t               f            s
                        |               |            |
                    two three       four five    six seven

the idea is to move a 'pointer' when there is a partial match on either
of the valid nodes. invalid combinations reset the pointer to the root.
A valid match is when there is no more depth.

too bad that this is not imperative code making this 1000x harder.

-}

data EnglishNumberTree a b
    = Node a [EnglishNumberTree a b]
    | Leaf b
    deriving (Show)

englishNumberTree :: EnglishNumberTree String Int
englishNumberTree =
    Node "#" --symbols aren't in the input set so it's okay to use symbols to represent root
        [ 
          Node "one" [Leaf 1],
          Node "eight" [Leaf 8],
          Node "nine" [Leaf 9],
          Node "t"
            [
              Node "two" [Leaf 2],
              Node "three" [Leaf 3]
            ],
          Node "s"
            [
              Node "six" [Leaf 6],
              Node "seven" [Leaf 7]
            ],
          Node "f"
            [
              Node "four" [Leaf 4],
              Node "five" [Leaf 5]
            ]
        ]

hsilgneNumberTree :: EnglishNumberTree String Int
hsilgneNumberTree =
  Node "#" --symbols aren't in the input set so it's okay to use symbols to represent root
    [ 
      Node "owt" [Leaf 2],
      Node "ruof" [Leaf 4],
      Node "xis" [Leaf 6],
      Node "neves" [Leaf 7],
      Node "thgie" [Leaf 8],
      Node "e"
        [
          Node "eno" [Leaf 1],
          Node "eerht" [Leaf 3],
          Node "evif" [Leaf 5],
          Node "enin" [Leaf 9]
        ]
    ]

--findNumEng :: String -> EnglishNumberTree -> String
--findNumEng xs tree = findNumEng' xs tree ""
--  where
--    findNumEng' :: String -> EnglishNumberTree -> String -> String
--    findNumEng' [] _ _ = ""
--    findNumEng' (x:xs) tree partial =
--      if isDigit x then x
--      else











-- if no partial match, return the root tree
-- if partial match while on a root node, return the node with the partial match
-- if partial match on current, non-root node, do nothing.
-- if complete match on current, non-root node, either return the node (and recurse) or the value of the leaf connected to the node.

bfs :: EnglishNumberTree a b -> [b]
bfs tree = bfs' [tree] [] -- Can't use recursion here. We need two wrap in a list so that the cons : operator becomes available.
  where
    bfs' :: [EnglishNumberTree a b] -> [b] -> [b]
    bfs' [] acc = acc
    bfs' (Node value children : rest) acc = bfs' (rest ++ children) (acc) -- in the first arg we repeat but with the current node removed. the children of the node are moved at the end (because we want breadth). the second arg is the concatenator
    bfs' (Leaf value : rest) acc = bfs' rest (acc ++ [value]) -- for demonstration we actually aren't resolving the mapping like the answer asks.


findNumEng :: String -> EnglishNumberTree String Int -> String
findNumEng xs tree = ""


-- same as before, mostly. this time we provide the default tree, both normal and reversed, for first and last.
concatFirstLastEng :: String -> Int
concatFirstLastEng xs = 
  let first = findNumEng xs englishNumberTree
  in
    if first == "" then 0 -- Short circuit. If first is empty, then there is no last. Therefore sum is 0.
    else
      let
        last = findNumEng (reverse xs) hsilgneNumberTree
        z = strToInt ([head first] ++ [head last]) -- "1abcd" and "9zyxw" -> "19"
      in
        z

-- this function is like the last one except different function call. we're still removing head strings from the list. nothing hairy yet.
sumFirstLastLinesEng :: [String] -> Int
sumFirstLastLinesEng xs = sumFirstLastLinesEng' xs 0
  where
    sumFirstLastLinesEng' :: [String] -> Int -> Int
    sumFirstLastLinesEng' [] acc = acc
    sumFirstLastLinesEng' (x:xs) acc = sumFirstLastLinesEng' xs (acc + concatFirstLastEng x) -- tail recursion accumulating the lines.


day1b :: IO [String]
day1b = do
    file <- readFile "day1.txt"
    let flines = lines file
    return [show $ sumFirstLastLinesEng flines]
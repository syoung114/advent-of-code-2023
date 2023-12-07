module Day1 where

import Data.Char
import Data.List

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
      strToInt ([head first] ++ [head last]) -- "1abcd" and "9zyxw" -> "19"
      where
        last = trimStringUntil isDigit (reverse xs)

sumFirstLastLines :: [String] -> Int
sumFirstLastLines xs = foldl (\acc x -> acc + concatFirstLast x) 0 xs

day1a :: IO [String]
day1a = do
    file <- readFile "../input/day1.txt"
    let flines = lines file
    return [show $ sumFirstLastLines flines]

-- part 2 (attempt 1)-----------------------------------------------------------

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

or just find + replace lolz

-}

data EnglishNumberTree a b
    = Node a [EnglishNumberTree a b]
    | Leaf b
    deriving (Show, Eq)

englishNumberTree :: EnglishNumberTree String Int
englishNumberTree =
    Node "#" --symbols aren't in the input set so it's okay to use symbols to represent root
        [ 
          Node "o"
            [
              Node "n"
                [
                  Node "e" [Leaf 1]
                ]
            ],
          Node "e"
            [
              Node "i"
                [
                  Node "g"
                    [
                      Node "h"
                        [
                          Node "t" [Leaf 8]
                        ]
                    ]
                ]
            ],
          Node "n"
            [
              Node "i"
                [
                  Node "n"
                    [
                      Node "e" [Leaf 9]
                    ]
                ]
            ],
          Node "t"
            [
              Node "w"
                [
                  Node "o" [Leaf 2]
                ],
              Node "h"
                [
                  Node "r"
                  [
                    Node "e"
                      [
                        Node "e" [Leaf 3]
                      ]
                  ]
                ]
            ],
          Node "s"
            [
              Node "i"
                [
                  Node "x" [Leaf 6]
                ],
              Node "e"
                [
                  Node "v"
                    [
                      Node "e"
                        [
                          Node "n" [Leaf 7]
                        ]
                    ]
                ]
            ],
          Node "f"
            [
              Node "o"
                [
                  Node "u"
                    [
                      Node "r" [Leaf 4]
                    ]
                ],
              Node "i"
                [
                  Node "v"
                    [
                      Node "e" [Leaf 5]
                    ]
                ]
            ]
        ]

hsilgneNumberTree :: EnglishNumberTree String Int
hsilgneNumberTree =
  Node "#" --symbols aren't in the input set so it's okay to use symbols to represent root
    [ 
      Node "o"
        [
          Node "w"
            [
              Node "t" [Leaf 2]
            ]
        ],
      Node "r"
        [
          Node "u"
            [
              Node "o"
                [
                  Node "f" [Leaf 4]
                ]
            ]
        ],
      Node "x"
        [
          Node "i"
            [
              Node "s" [Leaf 6]
            ]
        ],
      Node "n"
        [
          Node "e"
            [
              Node "v"
                [
                  Node "e"
                    [
                      Node "s" [Leaf 7]
                    ]
                ]
            ]
        ],
      Node "t"
        [
          Node "h"
            [
              Node "g"
                [
                  Node "i"
                    [
                      Node "e" [Leaf 8]
                    ]
                ]
            ]
        ],
      Node "e"
        [
          Node "e"
            [
              Node "r"
                [
                  Node "h"
                    [
                      Node "t" [Leaf 3]
                    ]
                ]
            ],
          Node "v"
            [
              Node "i"
                [
                  Node "f" [Leaf 5]
                ]
            ],
          Node "n"
            [
              Node "o" [Leaf 1],
              Node "i"
                [
                  Node "n" [Leaf 9]
                ]
            ]
        ]
    ]


-- if no partial match, return the root tree
-- if partial match while on a root node, return the node with the partial match
-- if partial match on current, non-root node, do nothing.
-- if complete match on current, non-root node, either return the node (and recurse) or the value of the leaf connected to the node.

traverseUntil :: [a] -> a -> (a -> Bool) -> (a -> a) -> ( a , Bool )
traverseUntil[] defaultVal _ _ = ( defaultVal , False )
traverseUntil (x:xs) defaultVal pred fn
  | pred x = ( fn x , True )
  | otherwise = traverseUntil xs defaultVal pred fn

matchWithTree :: String -> EnglishNumberTree String Int -> EnglishNumberTree String Int -> (EnglishNumberTree String Int, Bool)
matchWithTree partial root currentNode@(Node _ children) =
  traverseUntil children root (\(Node label _) -> isPrefixOf partial label)
  (
    \x@(Node label _) ->
      if length partial == length label then
        x -- this one looks interesting. move down the tree.
      else
        currentNode -- no match but no reason to reset yet.
  )

--character per string function
findNumEng :: String -> EnglishNumberTree String Int -> Int
findNumEng xs root = findNumEng' xs "" root root
  where
    findNumEng' :: String -> String -> EnglishNumberTree String Int -> EnglishNumberTree String Int -> Int
    findNumEng' _ _ _ (Node _ [Leaf val]) = val
    findNumEng' [] _ _ _ = 0 -- happens when matchWithTree reaches the end of its search for matches.
    findNumEng' (x:xs) prev root current 
      | isDigit x = strToInt [x] --note that this ignores the progress of building the partial string.
      | otherwise =
          let
            xStr = [x]
            n = matchWithTree xStr root current --search the depth below.
          in
            if snd n then
              findNumEng' xs xStr root (fst n) -- everything going as planned. We found the next valid character. Go deeper in tree.
            else
              let
                retryPivot = matchWithTree xStr root root
                relativePivot = matchWithTree prev root root
              in
                if root /= current then
                  if snd retryPivot then -- if string is something like "nnine"
                    findNumEng' (x:xs) "" root root
                  else if snd relativePivot && (fst relativePivot) /= current then -- if string is like "ninine". given it is relative to the current node, we must check if what we find is not the same node we have.
                    findNumEng' ((prev++xStr)++xs) "" root root
                  else
                    findNumEng' xs "" root root
                else
                  findNumEng' xs "" root root

-- same as before, mostly. this time we provide the default tree, both normal and reversed, for first and last.
concatFirstLastEng :: String -> Int
concatFirstLastEng xs
  | first == 0 = 0 -- Short circuit. If first is empty, then there is no last. Therefore sum is 0.
  | otherwise = strToInt (show first ++ show (
      if second == 0 then
        first
      else
        second
    ))
  where
    first = findNumEng xs englishNumberTree
    second = findNumEng (reverse xs) hsilgneNumberTree

-- this function is like the last one except different function call. we're still removing head strings from the list. nothing hairy yet.
sumFirstLastLinesEng :: [String] -> Int
sumFirstLastLinesEng xs = foldl (\acc x -> acc + concatFirstLastEng x) 0 xs

day1b :: IO [String]
day1b = do
    file <- readFile "../input/day1.txt"
    let flines = lines file
    return [show $ sumFirstLastLinesEng flines]
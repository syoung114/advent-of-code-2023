module Day7 where

import Data.Char (ord)
import Data.List (group, sort, sortBy, groupBy)
import Data.List.Split (splitOn)

data Card = NumberCard Int | T | J | Q | K | A
  deriving (Show, Eq, Ord)

type Hand = (Card, Card, Card, Card, Card)

data HandValue = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Show, Eq, Ord, Enum)

handToList :: Hand -> [Card]
handToList (a,b,c,d,e) = [a,b,c,d,e]

listToHand :: [Card] -> Hand
listToHand (a:b:c:d:e:_) = (a,b,c,d,e)

compareHand :: Hand -> Hand -> Ordering
compareHand x y = compareHand' (handToList x) (handToList y)
  where
    compareHand' :: [Card] -> [Card] -> Ordering
    compareHand' [] [] = EQ
    compareHand' x y
      | x > y = GT
      | x < y = LT
      | otherwise = compareHand' (tail x) (tail y)

handOrdValue :: Hand -> HandValue
handOrdValue x = handOrdValue' $ handOrd x
  where
    handOrd :: Hand -> [Int]
    handOrd x = sort $ map length (group $ sort $ handToList x)

    handOrdValue' :: [Int] -> HandValue
    handOrdValue' hand
      | hand == [5] = FiveOfAKind
      | hand == [1,4] = FourOfAKind
      | hand == [2,3] = FullHouse
      | hand == [1,1,3] = ThreeOfAKind
      | hand == [1,2,2] = TwoPairs
      | hand == [1,1,1,2] = OnePair
      | otherwise = HighCard

appraise :: (Hand, Int) -> Int
appraise (cards,bid) = (fromEnum $ handOrdValue cards) * bid

totalValue :: [(Hand, Int)] -> Int
totalValue xs =
  fst $ foldl (\(acc, i) x -> (acc + x*i, i+1)) (0, 1)
  (
    map snd
      $ concat
        $ map
        (
          sortBy (\x y -> compareHand (fst x) (fst y))
        )
        $ groupBy
          (
            \x y ->
              (handOrdValue $ fst x) == (handOrdValue $ fst y)
          )
          $ sortBy (\x y -> compare (handOrdValue $ fst x) (handOrdValue $ fst y)) xs
  )
--  foldl
--  (\acc x -> acc + appraise x) 0
--  (
--    map (\x -> zip (handOrdValue $ fst x) x) xs
--  )

reformat :: [String] -> [(Hand, Int)]
reformat xs = map (\x -> tuple (strToHand $ head $ splt x) (strToInt $ last $ splt x)) xs
  where
    tuple :: a -> b -> (a , b)
    tuple a b = (a , b)

    splt :: String -> [String]
    splt xs = splitOn " " xs

    strToInt :: String -> Int
    strToInt xs = read xs :: Int

    strToHand :: String -> Hand
    strToHand (a:b:c:d:e:_) = (charToCard a , charToCard b , charToCard c , charToCard d , charToCard e)

    charToCard :: Char -> Card
    charToCard 'T' = T
    charToCard 'J' = J
    charToCard 'Q' = Q
    charToCard 'K' = K
    charToCard 'A' = A
    charToCard x = NumberCard (digitToInt x)

    digitToInt :: Char -> Int
    digitToInt x = ord x - ord '0'

day7a :: IO [String]
day7a = do
  contents <- readFile "../input/day7.txt"
  let flines = lines contents
      input = reformat flines
  return [show $ totalValue input]
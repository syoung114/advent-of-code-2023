module Day7 where

import Data.Char (ord)
import Data.List (group, sort, sortBy, groupBy, partition)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

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

-- Calculate the total value of hands based on their hand value and bid
totalValue :: [(Hand, Int)] -> Int
totalValue xs =
  fst
    $ foldl (\(acc, i) x -> (acc + x*i, i+1)) (0, 1) -- Accumulate the total value by multiplying each bid with its corresponding index
    $ map snd -- Extract the bids from the list of (Hand, Int) tuples
      $ concat -- Flatten the list of lists (remember that groupBy returns a list of lists). Because arranging is done we don't need granularity anymore.
      $ map
        (
          sortBy (\x y -> compareHand (fst x) (fst y)) -- Sort inside the list of tuples to arrange the absolute best and worst hands
        )
      $ groupBy
        (
          \x y ->
            (handOrdValue $ fst x) == (handOrdValue $ fst y) -- Group the tuples with the same hand ordinal value
        )
      $ sortBy (\x y -> compare (handOrdValue $ fst x) (handOrdValue $ fst y)) xs -- Sort the tuples based on the hand value using handOrdValue function

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

-- part 2 ----------------------------------------------------------------------

data Card2 = J2 | NumberCard2 Int | T2 | Q2 | K2 | A2
  deriving (Show, Eq, Ord)

type Hand2 = (Card2, Card2, Card2, Card2, Card2)

data HandValue2 = HighCard2 | OnePair2 | TwoPairs2 | ThreeOfAKind2 | FullHouse2 | FourOfAKind2 | FiveOfAKind2
  deriving (Show, Eq, Ord, Enum)

handToList2 :: Hand2 -> [Card2]
handToList2 (a,b,c,d,e) = [a,b,c,d,e]

listToHand2 :: [Card2] -> Hand2
listToHand2 (a:b:c:d:e:_) = (a,b,c,d,e)

compareHand2 :: Hand2 -> Hand2 -> Ordering
compareHand2 x y = compareHand' (handToList2 x) (handToList2 y)
  where
    compareHand' :: [Card2] -> [Card2] -> Ordering
    compareHand' [] [] = EQ
    compareHand' x y
      | x > y = GT
      | x < y = LT
      | otherwise = compareHand' (tail x) (tail y)

handOrdValue2 :: Hand2 -> HandValue2
handOrdValue2 x = handOrdValue' $ handOrd x
  where
    handOrd :: Hand2 -> [Int]
    handOrd x =
      sort
      $ map length
        (
          if lenOthers == -1 then --actually checking if there are no `others`. 
            [jokers]
          else
            ((take lenOthers others) ++ [(head $ drop lenOthers others) ++ jokers]) -- Push jokers into the best hand group
        )
        where
          (jokers, others) =
            let 
              (jokers_,others_) = partition (== J2) (handToList2 x)
            in
              (
                jokers_, 
                sortBy (comparing length) $ group $ sort others_
              )
          lenOthers = length others - 1

    handOrdValue' :: [Int] -> HandValue2
    handOrdValue' hand
      | hand == [5] = FiveOfAKind2
      | hand == [1,4] = FourOfAKind2
      | hand == [2,3] = FullHouse2
      | hand == [1,1,3] = ThreeOfAKind2
      | hand == [1,2,2] = TwoPairs2
      | hand == [1,1,1,2] = OnePair2
      | otherwise = HighCard2

-- Calculate the total value of hands based on their hand value and bid
totalValue2 :: [(Hand2, Int)] -> Int
totalValue2 xs =

  fst
    $ foldl (\(acc, i) x -> (acc + x*i, i+1)) (0, 1) -- Accumulate the total value by multiplying each bid with its corresponding index
    $ map snd -- Extract the bids from the list of (Hand, Int) tuples
      $ 
      
      concat -- Flatten the list of lists (remember that groupBy returns a list of lists). Because arranging is done we don't need granularity anymore.
      $ 
      map
        (
          sortBy (\x y -> compareHand2 (fst x) (fst y)) -- Sort inside the list of tuples based on the hand value using compareHand function
        )
      $ groupBy
        -- groupBy
        (
          \x y ->
            (handOrdValue2 $ fst x) == (handOrdValue2 $ fst y) -- Group the tuples with the same hand value
        )
      $ sortBy (\x y -> compare (handOrdValue2 $ fst x) (handOrdValue2 $ fst y)) xs -- Sort the tuples based on the hand value using handOrdValue function

reformat2 :: [String] -> [(Hand2, Int)]
reformat2 xs = map (\x -> tuple (strToHand2 $ head $ splt x) (strToInt $ last $ splt x)) xs
  where
    tuple :: a -> b -> (a , b)
    tuple a b = (a , b)

    splt :: String -> [String]
    splt xs = splitOn " " xs

    strToInt :: String -> Int
    strToInt xs = read xs :: Int

    strToHand2 :: String -> Hand2
    strToHand2 (a:b:c:d:e:_) = (charToCard a , charToCard b , charToCard c , charToCard d , charToCard e)

    charToCard :: Char -> Card2
    charToCard 'T' = T2
    charToCard 'J' = J2
    charToCard 'Q' = Q2
    charToCard 'K' = K2
    charToCard 'A' = A2
    charToCard x = NumberCard2 (digitToInt x)

    digitToInt :: Char -> Int
    digitToInt x = ord x - ord '0'


day7b :: IO [String]
day7b = do
  contents <- readFile "../input/day7.txt"
  let flines = lines contents
      input = reformat2 flines
  return [show $ totalValue2 input]

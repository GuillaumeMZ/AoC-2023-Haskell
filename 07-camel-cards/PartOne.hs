import           Data.List (group, sort, sortBy)
import           Data.Sequence (mapWithIndex)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

data Card = 
      Two 
    | Three 
    | Four 
    | Five 
    | Six 
    | Seven 
    | Eight 
    | Nine 
    | T 
    | J 
    | Q 
    | K 
    | A 
    deriving(Show, Eq, Ord)

data HandType = 
      HighCard Card Card Card Card Card
    | OnePair Card Card Card Card Card
    | TwoPairs Card Card Card Card Card
    | ThreeOfAKind Card Card Card Card Card
    | FullHouse Card Card Card Card Card
    | FourOfAKind Card Card Card Card Card
    | Ace Card Card Card Card Card
    deriving(Show, Eq, Ord)

data Hand = Hand {
    handType :: HandType,
    bid :: Int
} deriving(Show)

-- assuming length cards == 5
cardsToHandType :: [Card] -> HandType
cardsToHandType cards = 
    let cardsGroupsLengthsOrdered = reverse . sort $ map length $ group . sort $ cards
        ctor = case cardsGroupsLengthsOrdered of
                [5] -> Ace
                [4, 1] -> FourOfAKind
                [3, 2] -> FullHouse
                [3, 1, 1] -> ThreeOfAKind
                [2, 2, 1] -> TwoPairs
                [2, 1, 1, 1] -> OnePair
                [1, 1, 1, 1, 1] -> HighCard
        in ctor (cards !! 0) (cards !! 1) (cards !! 2) (cards !! 3) (cards !! 4)


parseCard :: Char -> Card
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = T
parseCard 'J' = J
parseCard 'Q' = Q
parseCard 'K' = K
parseCard 'A' = A
parseCard _ = undefined

type Line = T.Text
type Paragraph = [T.Text]

parseHand :: Line -> Hand
parseHand line = 
    let cards = map parseCard $ T.unpack $ T.take 5 line
        bid   = (read :: String -> Int) . T.unpack . T.drop 6 $ line
    in Hand (cardsToHandType cards) bid

totalWinnings :: [Hand]  -> Int
totalWinnings hands = sum $ map (\(a, b) -> a * bid b) . zip [1..] . sortBy (\a b -> handType a `compare` handType b) $ hands

main :: IO ()
main = do
    input <- T.IO.getContents
    print $ totalWinnings $ map parseHand $ T.lines input
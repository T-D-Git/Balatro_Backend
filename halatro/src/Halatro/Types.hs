module Halatro.Types where

--------------------------------------------------------------------------------

-- | The rank of a card, from Two to Ace
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show :: Rank -> String
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "Jack"
  show Queen = "Queen"
  show King = "King"
  show Ace = "Ace"

-- | A shorter version of the rank, for use in the Terminal UI.
shortRank :: Rank -> String
shortRank Two = "2"
shortRank Three = "3"
shortRank Four = "4"
shortRank Five = "5"
shortRank Six = "6"
shortRank Seven = "7"
shortRank Eight = "8"
shortRank Nine = "9"
shortRank Ten = "10"
shortRank Jack = "J"
shortRank Queen = "Q"
shortRank King = "K"
shortRank Ace = "A"

--------------------------------------------------------------------------------

-- | The suit of a card
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | This will only render properly for fonts that include the Unicode card
--     suits, which *should* be most of them, but Windows command prompt might
--     get weird about it.
shortSuit :: Suit -> String
shortSuit Clubs = "♣"
shortSuit Diamonds = "♦"
shortSuit Hearts = "♥"
shortSuit Spades = "♠"

--------------------------------------------------------------------------------

-- | A playing card is made up of a rank and a suit (e.g. the 2 of Clubs)
data Card = Card Rank Suit
  deriving (Eq, Ord)

rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

instance Show Card where
  show :: Card -> String
  show (Card r s) = show r ++ " of " ++ show s

--------------------------------------------------------------------------------

-- | We add a convenience type synonym for a hand of cards.
type Hand = [Card]

-- | Render out a list of card names in a longform but readable format
printHand :: Hand -> String
printHand [] = "no cards"
printHand [c1] = show c1
printHand [c1, c2] = show c1 ++ " and " ++ show c2
printHand [c1, c2, c3] = show c1 ++ ", " ++ show c2 ++ ", and " ++ show c3
printHand (c : cs) = show c ++ ", " ++ printHand cs

--------------------------------------------------------------------------------

-- |
--  Every hand has a "type" which is the highest value hand contained within it.
--
--  In the full game of Balatro, there are other hand types, but we'll stick to
--  ones that can be made from a normal deck of cards.
data HandType
  = None
  | HighCard
  | Pair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush
  deriving (Eq, Ord, Enum, Bounded)

instance Show HandType where
  show :: HandType -> String
  show None = "None"
  show HighCard = "High Card"
  show Pair = "Pair"
  show TwoPair = "Two Pair"
  show ThreeOfAKind = "Three of a Kind"
  show Straight = "Straight"
  show Flush = "Flush"
  show FullHouse = "Full House"
  show FourOfAKind = "Four of a Kind"
  show StraightFlush = "Straight Flush"
  show RoyalFlush = "Royal Flush"

type Chips = Int

type Mult = Int

--------------------------------------------------------------------------------
-- Types for the AI

data PlayOrDiscard = Play | Discard
  deriving (Eq, Show)

data Move = Move PlayOrDiscard [Card]
  deriving (Eq, Show)

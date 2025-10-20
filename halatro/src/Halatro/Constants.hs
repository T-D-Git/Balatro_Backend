module Halatro.Constants where

import Halatro.Types

-- | We will hard-code the hand size to 8.
handSize :: Int
handSize = 8

handTypeValues :: HandType -> (Chips, Mult)
handTypeValues None = (0, 0)
handTypeValues HighCard = (5, 1)
handTypeValues Pair = (10, 2)
handTypeValues TwoPair = (20, 2)
handTypeValues ThreeOfAKind = (30, 3)
handTypeValues Straight = (30, 4)
handTypeValues Flush = (35, 4)
handTypeValues FullHouse = (40, 4)
handTypeValues FourOfAKind = (60, 7)
handTypeValues StraightFlush = (100, 8)
handTypeValues RoyalFlush = (100, 8)

rankScore :: Rank -> Int
rankScore Ace = 11
rankScore King = 10
rankScore Queen = 10
rankScore Jack = 10
rankScore Ten = 10
rankScore Nine = 9
rankScore Eight = 8
rankScore Seven = 7
rankScore Six = 6
rankScore Five = 5
rankScore Four = 4
rankScore Three = 3
rankScore Two = 2
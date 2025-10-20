{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-all #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Fuse concatMap/map" #-}
{-# HLINT ignore "Use map once" #-}

-- |
-- You are not required to edit this file, but do feel free to take a look. The
-- code in here is implemented in a deliberately poor way, without comments or
-- guidance. In my opinion, decoding the approaches taken here is *much* more
-- difficult than implementing the work properly yourself :)
module Main where

{-
Tasty is the testing library that is used to specify tests.
The backends "tasty-hunit" and "tasty-quickcheck" specify the way that unit
tests and property tests (respectively) are written.
-}

import Control.Arrow
import Control.Monad
import CourseworkOne
import Data.Bifunctor (bimap)
import Data.Bool
import Data.Function
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Ratio
import Data.Set qualified as Set
import Halatro.BasicGame
import Halatro.Constants
import Halatro.Types
import System.Console.ANSI
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
import Test.Tasty.Muffled (muffledMain)
import Test.Tasty.QuickCheck hiding (Discard)
import Text.Printf

main :: IO ()
main = do
  clearScreen
  muffledMain $
    testGroup
      "Tests"
      [ testPartOne,
        testPartTwo,
        testPartThree,
        testPartFour,
        testPartFive
      ]

testPartOne :: TestTree
testPartOne =
  testGroup
    "Part One"
    [ testGroup "Ex. 1: contains" $
        let testCases =
              [ ( HighCard,
                  [[2 @ H, 3 @ D, 4 @ H, 5 @ H, 7 @ H]],
                  [[2 @ H, 2 @ D, 3 @ H, 4 @ H, 5 @ H]]
                ),
                ( Pair,
                  [ [2 @ H, 2 @ D, 3 @ H, 4 @ H, 5 @ H],
                    [A @ H, A @ D, A @ H, 2 @ H, 3 @ H]
                  ],
                  [[2 @ H, 3 @ D, 4 @ H, 5 @ H, 6 @ H]]
                ),
                ( TwoPair,
                  [ [2 @ H, 2 @ D, 3 @ H, 3 @ D, 5 @ H],
                    [2 @ H, 2 @ D, 3 @ H, 3 @ D, 3 @ C]
                  ],
                  [[2 @ H, 3 @ D, 4 @ H, 5 @ H, 6 @ H]]
                ),
                ( ThreeOfAKind,
                  [[2 @ H, 2 @ D, 2 @ C, 3 @ D, 5 @ H]],
                  [[2 @ H, 3 @ D, 4 @ H, 5 @ H, 6 @ H]]
                ),
                ( FourOfAKind,
                  [[2 @ H, 2 @ D, 2 @ C, 2 @ S, 5 @ H]],
                  [[2 @ H, 3 @ D, 4 @ H, 5 @ H, 6 @ H]]
                ),
                ( Straight,
                  [ [2 @ H, 3 @ D, 4 @ C, 5 @ S, 6 @ H],
                    [10 @ H, J @ H, Q @ H, K @ H, A @ H]
                  ],
                  [[J @ H, Q @ D, K @ H, A @ H, 2 @ H]]
                ),
                ( Flush,
                  [ [2 @ H, 3 @ H, 4 @ H, 5 @ H, 6 @ H],
                    [10 @ H, J @ H, Q @ H, K @ H, A @ H]
                  ],
                  [[2 @ H, 3 @ D, 4 @ H, 5 @ H, 6 @ H]]
                ),
                ( FullHouse,
                  [[2 @ H, 2 @ D, 2 @ C, 3 @ D, 3 @ H]],
                  [ [2 @ H, 3 @ D, 4 @ H, 5 @ H, 6 @ H],
                    [2 @ H, 2 @ D, 2 @ D, 3 @ C, 4 @ S]
                  ]
                ),
                ( StraightFlush,
                  [ [2 @ H, 3 @ H, 4 @ H, 5 @ H, 6 @ H],
                    [10 @ H, J @ H, Q @ H, K @ H, A @ H]
                  ],
                  [[2 @ H, 3 @ D, 4 @ H, 5 @ H, 6 @ H]]
                ),
                ( RoyalFlush,
                  [[10 @ H, J @ H, Q @ H, K @ H, A @ H]],
                  [ [2 @ H, 3 @ D, 4 @ H, 5 @ H, 6 @ H],
                    [Q @ H, K @ H, A @ H, 2 @ H, 3 @ H]
                  ]
                )
              ]
            noneCases =
              testGroup
                "None"
                [ testGroup
                    "gives False for..."
                    $ pure
                    $ testCase (show [2 @ C])
                    $ [2 @ C] `contains` None @?= False,
                  testGroup
                    "gives True for..."
                    $ pure
                    $ testCase (printHand [])
                    $ [] `contains` None @?= True
                ]
            nonNoneCases = flip map testCases $ \(ht, yesCases, noCases) -> do
              testGroup
                (show ht)
                [ testGroup "gives True for..." $ do
                    flip map yesCases $ \hand ->
                      do
                        testCase (printShortHand hand)
                        $ hand `contains` ht @?= True,
                  testGroup "gives False for..." $ do
                    flip map noCases $ \hand ->
                      do
                        testCase (printShortHand hand)
                        $ hand `contains` ht @?= False
                ]
         in nonNoneCases ++ [noneCases]
    ]

testPartTwo :: TestTree
testPartTwo =
  testGroup
    "Part Two"
    [ testGroup "Ex. 2: bestHandType" $
        let testCases =
              [ (RoyalFlush, [10 @ H, J @ H, Q @ H, K @ H, A @ H]),
                (StraightFlush, [2 @ H, 3 @ H, 4 @ H, 5 @ H, 6 @ H]),
                (FourOfAKind, [2 @ H, 2 @ D, 2 @ C, 2 @ S, 5 @ H]),
                (FullHouse, [2 @ H, 2 @ D, 2 @ C, 3 @ D, 3 @ H]),
                (Flush, [2 @ H, 4 @ H, 6 @ H, 8 @ H, 10 @ H]),
                (Straight, [2 @ H, 3 @ D, 4 @ C, 5 @ S, 6 @ H]),
                (ThreeOfAKind, [2 @ H, 2 @ D, 2 @ C, 3 @ D, 5 @ H]),
                (TwoPair, [2 @ H, 2 @ D, 3 @ H, 3 @ D, 5 @ H]),
                (Pair, [2 @ H, 2 @ D, 3 @ H, 4 @ H, 5 @ H]),
                (HighCard, [2 @ H, 3 @ D, 4 @ H, 5 @ H, 7 @ H])
              ]
            longestCardName = maximum $ map (length . printShortHand . snd) testCases
            pad s = s <> replicate (longestCardName - length s) ' '
         in map
              ( \(ht, hand) ->
                  testCase (pad (printShortHand hand) <> " ==> " <> show ht) $
                    bestHandType hand @?= ht
              )
              testCases
    ]

testPartThree :: TestTree
testPartThree =
  testGroup
    "Part Three"
    [ testGroup
        "Ex. 3: whichCardsScore"
        [ testProperty "For 5-card hand types, all cards score"
            $ forAllShow
              (vector 5 `suchThat` (\h -> bestHandType h `elem` [RoyalFlush, StraightFlush, FullHouse, Flush, Straight]))
              printShortHand
            $ \x -> whichCardsScore x `eqSet` x,
          scoringProp
            "For 4-card hand types, four cards should be scored"
            [FourOfAKind, TwoPair]
            4,
          scoringProp
            "For 3-of-a-kind, three cards should be scored"
            [ThreeOfAKind]
            3,
          scoringProp
            "For pair, two cards should be scored"
            [Pair]
            2,
          scoringProp
            "For high card, only one card is scored"
            [HighCard]
            1,
          testCase "For an empty hand, no cards are scored" $
            whichCardsScore [] @=? []
        ],
      testGroup
        "Ex. 4: scoreHand"
        [ testCase ("Works on the example in the spec") $
            scoreHand [A @ S, A @ H, A @ C, 3 @ C, 2 @ H] @?= 189,
          testProperty "Works on 1000 random hands" $
            withMaxSuccess 1000 $ do
              n <- chooseInt (1, 5)
              hand <- vector n
              pure $ scoreHand hand `shouldScore` sw hand
        ]
    ]
  where
    scoringProp str types n =
      testProperty
        str
        $ forAllShow
          (vector 5 `suchThat` (\h -> bestHandType h `elem` types && List.nub h == h))
          printShortHand
          (\x -> x `scoreNCards` n)

sameRanks :: [Rank] -> [Rank] -> Property
x `sameRanks` y =
  counterexample
    (concat ["Should give: ", show y, " (in some order) \nBut got:     ", show x])
    res
  where
    res = List.sort x == List.sort y
    interpret True = " == "
    interpret False = " /= "

eqSet :: Hand -> Hand -> Property
x `eqSet` y =
  counterexample
    (concat ["Should give: ", printShortHand y, "\nBut got:     ", printShortHand x])
    res
  where
    res = List.sort x == List.sort y
    interpret True = " == "
    interpret False = " /= "

x `shouldScore` y = counterexample ("Should score: " ++ show y ++ "\nBut got:      " ++ show x) (x == y)

scoreNCards :: Hand -> Int -> Property
scoreNCards hand n =
  let w = whichCardsScore hand
   in counterexample
        ("Tried to score " ++ show (length w) ++ " cards: " ++ printShortHand w)
        (length w == n)

testPartFour :: TestTree
testPartFour =
  testGroup
    "Part Four"
    [ testGroup
        "Ex. 5: highestScoringHand"
        [ testProperty
            -- ("Example 1 (" ++ printShortHand [2 @ H, 3 @ H, 4 @ H, 5 @ H, 6 @ H, 7 @ H, 8 @ H, 9 @ H] ++ ")")
            "Example 1"
            $ highestScoringHand [2 @ H, 3 @ H, 4 @ H, 5 @ H, 6 @ H, 7 @ H, 8 @ H, 9 @ H] `eqSet` [5 @ H, 6 @ H, 7 @ H, 8 @ H, 9 @ H],
          testProperty
            "Example 2"
            -- ("Example 2 (" ++ printShortHand [2 @ H, 3 @ C, 5 @ H, 7 @ C, 8 @ D, 9 @ S, J @ C, Q @ D] ++ ")")
            $ counterexample " We should score the Queen, as it is the highest card and there is no better option"
            $ Q @ D `elem` highestScoringHand [2 @ H, 3 @ C, 5 @ H, 7 @ C, 8 @ D, 9 @ S, J @ C, Q @ D],
          testProperty
            "Always selects between one and five cards"
            $ forAll (listOf1 arbitrary `suchThat` (\h -> length h <= 8))
            $ \hand ->
              let h = highestScoringHand hand
               in length h >= 1 && length h <= 5,
          testProperty
            "A hand with fewer cards never has a better score"
            $ forAll
              ( do
                  x <- listOf1 arbitrary `suchThat` (\h -> length h <= 8)
                  x' <- sublistOf x
                  pure (x, x')
              )
            $ \(h, h') ->
              counterexample
                ("The highest scoring hand for " ++ printShortHand h ++ " was" ++ show (highestScoringHand h) ++ " but with the subset" ++ printShortHand h' ++ " it was " ++ show (highestScoringHand h'))
                (scoreHand (highestScoringHand h) >= scoreHand (highestScoringHand h')),
          testCase
            "Chooses an empty selection when the hand is empty"
            $ highestScoringHand [] @=? []
        ]
    ]

testPartFive :: TestTree
testPartFive =
  testGroup
    "Part Five"
    [ testGroup
        "Ex. 6: simpleAI"
        [ testProperty "Always plays the five highest cards" $
            forAll gamePosition $
              \(m, c) ->
                let Move m' c' = simpleAI m c
                 in map rank c' `sameRanks` map rank (take 5 (List.sortBy (\a b -> if a > b then LT else GT) c))
        ],
      testGroup
        "Ex. 7: sensibleAI"
        [ testProperty "Always plays the best hand (but doesn't discard)" $
            forAll gamePosition $
              \(m, c) ->
                let Move m' c' = sensibleAI m c
                 in scoreHand c' `shouldScore` scoreHand (highestScoringHand c)        
                 ],
      testGroup
        "Ex. 8: myAI"
        [ testCaseSteps "Plays Halatro" $
            \step -> do
              let numTries = 100
              avg <- getAverageDirect numTries
              let avgStr = printf "%.2f" (fromRational avg :: Double)
              step $ "\nAverage score over " ++ show numTries ++ " games: " ++ avgStr
        ]
    ]

move :: Gen Move
move = Move <$> elements [Play, Discard] <*> (vector =<< chooseInt (1, 5))

gamePosition :: Gen ([Move], [Card])
gamePosition = (,) <$> listOf1 move <*> vectorOf 8 arbitrary

getAverageDirect :: Int -> IO Rational
getAverageDirect n = do
  results <- replicateM n runAIDirectly
  pure $ fromIntegral (sum results) % fromIntegral n

--------------------------------------------------------------------------------
-- Helpers for computing scores of hands

-- NB. this is an atrocious way of doing this. Do not copy it. If you do, it
-- will be obvious, because there is no reason someone would write this code in
-- this way unless they were trying to be deliberately obtuse.
-- And obviously, do not use this directly or indirectly from your own code.

sw :: [Card] -> Int
sw h =
  whichCardsScore h
    & map (,1)
    & map (first rank)
    & Map.fromListWith (+)
    & Map.assocs
    & concatMap (uncurry $ flip replicate)
    & map rankScore
    & (,0)
    & first sum
    & let f = handTypeValues $ bestHandType h in uncurry (*) . pw (+) f
  where
    pw :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
    pw f (a, b) (c, d) = (f a c, f b d)

--------------------------------------------------------------------------------
-- Utilities to write test cases in a short format

-- Single-character names for suits and ranks
data ShortSuit = H | D | C | S deriving (Show, Eq)

data ShortRank = Val Int | J | Q | K | A deriving (Show, Eq)

-- Pro tip: don't do this
instance Num ShortRank where
  fromInteger :: Integer -> ShortRank
  fromInteger = Val . fromInteger

toLongSuit :: ShortSuit -> Suit
toLongSuit = \case H -> Hearts; D -> Diamonds; C -> Clubs; S -> Spades

toLongRank :: ShortRank -> Rank
toLongRank = \case
  Val i -> toEnum (i - 2)
  J -> Jack
  Q -> Queen
  K -> King
  A -> Ace

-- The @ operator is used to create a card in a more readable way
(@) :: ShortRank -> ShortSuit -> Card
r @ s = Card (toLongRank r) (toLongSuit s)

printShortHand :: Hand -> String
printShortHand = unwords . map shortCardName

shortCardName :: Card -> String
shortCardName (Card r s) = shortRank r ++ (shortSuit s)
  where
    c = case s of
      Hearts -> Red
      Diamonds -> Yellow
      Clubs -> Blue
      Spades -> Black

-- Use ANSI terminal colours to make the suits more readable
-- Unfortunately we can't use this as it messes with the testing framework's understanding of the output
withColor :: Color -> String -> String
withColor c s = setSGRCode [SetColor Foreground Dull c] ++ s ++ setSGRCode [Reset]

-- Generate cards arbitrarily for QuickCheck
instance Arbitrary Card where
  arbitrary :: Gen Card
  arbitrary = Card <$> arbitrary <*> arbitrary

instance Arbitrary Suit where
  arbitrary :: Gen Suit
  arbitrary = elements [Hearts, Diamonds, Clubs, Spades]

instance Arbitrary Rank where
  arbitrary :: Gen Rank
  arbitrary = elements [Two .. Ace]

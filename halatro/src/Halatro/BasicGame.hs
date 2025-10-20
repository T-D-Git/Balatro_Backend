{-# LANGUAGE MultiWayIf #-}

-- |
--     This module contains the code for running Halatro.
--     You do NOT need to worry about the contents of this file, but it
--     might be of interest to those of you looking to go deeper into advanced
--     Haskell programming.
--
--     Your code should go in src/CourseworkOne.hs.
module Halatro.BasicGame where

import CourseworkOne (myAI, scoreHand)
import Data.List qualified as List
import Data.Ord (Down (..), comparing)
import Halatro.Constants
import Halatro.Types
import System.Random (randomRIO)

-- | All the cards in a deck.
initialDeck :: [Card]
initialDeck = Card <$> [minBound .. maxBound] <*> [minBound .. maxBound]

-- initialDeck = [Card Two Hearts, Card Three Hearts, Card Four Hearts, Card Five Hearts, Card Six Hearts, Card Seven Hearts, Card Eight Hearts, Card Nine Hearts]

-- |
--   Perform a Fisher-Yates shuffle on the deck
--   https://en.wikipedia.org/wiki/Fisher-Yates_shuffle
--   N.B. NOT very efficient because of (++), but fine on 52 elements
--
--   "But Alex, Haskell functions are pure, and you said we can't do random
--   number generation" We'll get there! Don't worry about it for now :)
shuffleDeck :: [Card] -> IO [Card]
shuffleDeck [] = pure []
shuffleDeck [x] = pure [x]
shuffleDeck deck = do
  i <- randomRIO (0, length deck - 1)
  let (before, after) = splitAt i deck
  shuffled <- shuffleDeck (before ++ tail after)
  pure $ head after : shuffled

-- The current state of the game.
data GameState = GameState
  { roundScore :: Int,
    hand :: [Card],
    deck :: [Card],
    roundNum :: Int,
    handsRemaining :: Int,
    discardsRemaining :: Int,
    pastMoves :: [Move]
  }
  deriving (Show)

-- The initial state of the game.
initialGameState :: IO GameState
initialGameState = do
  deck <- shuffleDeck initialDeck
  pure
    GameState
      { roundScore = 0,
        hand = [],
        deck = deck,
        roundNum = 1,
        handsRemaining = 3,
        discardsRemaining = 3,
        pastMoves = []
      }

--------------------------------------------------------------------------------
-- Running the AI Directly

type AI = [Move] -> [Card] -> Move

runAIDirectly :: IO Int
runAIDirectly = runSomeAI myAI

runSomeAI :: AI -> IO Int
runSomeAI ai =
  do
    gs <- initialGameState >>= draw
    finalGS <- runLoopAI ai gs
    return $ roundScore finalGS

runLoopAI :: AI -> GameState -> IO GameState
runLoopAI ai gs = do
  if handsRemaining gs == 0
    then pure gs
    else do
      let move = ai (pastMoves gs) (hand gs)
      newGS <- applyMove move gs
      runLoopAI ai newGS

applyMove :: Move -> GameState -> IO GameState
applyMove m@(Move pord cards) gs@GameState {..} = do
  let newHand = hand List.\\ cards
      moveScore = case pord of
        Play -> scoreHand cards
        Discard -> 0
      newScore = roundScore + moveScore
      hr = case pord of
        Play -> handsRemaining - 1
        Discard -> handsRemaining
      dr = case pord of
        Play -> discardsRemaining
        Discard -> discardsRemaining - 1
      newGS =
        gs
          { hand = newHand,
            pastMoves = m : pastMoves,
            roundScore = newScore,
            handsRemaining = hr,
            discardsRemaining = dr
          }
  if
    | hr < 0 -> error "Tried to play a hand with 0 remaining hands"
    | dr < 0 -> error "Tried to discard with 0 remaining discards"
    | otherwise -> draw newGS

draw :: GameState -> IO GameState
draw gs@GameState {..} = do
  let (drawn, newDeck) = splitAt (handSize - length hand) deck
  pure
    gs
      { hand = List.sortBy (comparing Down) $ hand ++ drawn,
        deck = newDeck
      }
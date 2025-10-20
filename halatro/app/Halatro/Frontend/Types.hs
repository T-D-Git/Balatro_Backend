{- |
  This module contains the code for the terminal user interface (TUI) for
  Halatro. You do NOT need to worry about the contents of this file, but it
  might be of interest to those of you looking to go deeper into advanced
  Haskell programming.

  Your code should go in src/CourseworkOne.hs.
-}
module Halatro.Frontend.Types where

import Brick
import Halatro.BasicGame
import Halatro.Types

--------------------------------------------------------------------------------
-- TYPES

data TUIState = TUIState
  -- See "tick" for what the modes do
  { currentMode :: TUIMode
  , -- How long we are pausing before the next animation
    waiting :: Int
  , -- The selected hand type, if any
    selectedHandType :: Maybe HandType
  , -- The selected multiplier and chip count, if any
    selectedMult :: Maybe Int
  , selectedChips :: Maybe Int
  , -- Buckets for us to store cards in while performing animations
    cardsToDraw :: [Card]
  , cardsToDiscard :: [Card]
  , cardsDiscarded :: [Card]
  , cardsScored :: [Card]
  , cardsToScore :: [Card]
  , cardsToPlay :: [Card]
  , cardsInPlayArea :: [Card]
  , -- How many cards total have been discarded this game
    numDiscards :: Int
  , -- How many cards total have been played this game
    numCardsPlayed :: Int
  , -- Which cards are currently selected in the UI
    selectedCards :: [Card]
  , -- A visual representation of the hand score which can be animated and
    -- might lag behind the actual hand score
    logLines :: [Widget Name]
  , -- Whether a line was just added to the log
    justLogged :: Bool
  , -- True if the mouse button is currently down (avoids double clicking)
    clicked :: Bool
  , -- Whether the game should play itself using the provided AI
    isAIGame :: Bool
  , -- What the most recent AI move was
    aiMove :: Maybe Move
  , -- The basic game state is stored in this simpler datatype, which can be found in app/Halatro/BasicGame.hs. We keep a copy inside the TUI state.
    gameState :: GameState
  }

{- | The current mode of the TUI tells the program what it should do next.
See "tick" below for how the modes are used.
-}
data TUIMode
  = PlayCards
  | WaitDraw
  | WaitPlay
  | WaitScore
  | WaitUnscore
  | WaitDiscard
  | WaitEmptyPlayArea
  | Initial
  | GameOver
  deriving (Eq)

-- A simple unit type which the terminal receives 10 times per second, to force an update
data Tick = Tick

makeInitialTUIState :: Bool -> IO TUIState
makeInitialTUIState aiModeRequested = do
  gs <- initialGameState
  pure $
    TUIState
      { selectedHandType = Nothing
      , gameState = gs
      , selectedMult = Nothing
      , selectedChips = Nothing
      , selectedCards = []
      , logLines = []
      , justLogged = False
      , clicked = False
      , cardsToDraw = []
      , currentMode = Initial
      , cardsToDiscard = []
      , cardsDiscarded = []
      , cardsToPlay = []
      , cardsInPlayArea = []
      , cardsScored = []
      , cardsToScore = []
      , waiting = 0
      , numDiscards = 0
      , numCardsPlayed = 0
      , isAIGame = aiModeRequested
      , aiMove = Nothing
      }

--------------------------------------------------------------------------------

-- A Name is a unique identifier for each interactable widget in the UI
data Name
  = LogViewport
  | RenderedCard Int
  | PlayBtn
  | DiscardBtn
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Helpers

type GameUpdate = EventM Name TUIState ()

modifyGameState :: (GameState -> GameState) -> GameUpdate
modifyGameState f = modify $ \t -> t{gameState = f (gameState t)}
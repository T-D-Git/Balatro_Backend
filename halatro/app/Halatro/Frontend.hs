{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the code for the terminal user interface (TUI) for
--     Halatro. You do NOT need to worry about the contents of this file, but it
--     might be of interest to those of you looking to go deeper into advanced
--     Haskell programming.
--
--     Your code should go in src/CourseworkOne.hs.
module Halatro.Frontend where

import Brick hiding (Down)
import Brick.BChan qualified as Brick
import Brick.Widgets.Border
import Control.Applicative (asum)
import Control.Concurrent
import Control.Monad (forever, replicateM, void, when)
import Control.Monad.IO.Class (liftIO)
import CourseworkOne qualified as CW
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text qualified as Text
import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Halatro.BasicGame
import Halatro.Constants
import Halatro.Frontend.Types
import Halatro.Frontend.Utils
import Halatro.Frontend.Widgets
import Halatro.Types
import System.Environment
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- MAIN

-- | This is the main entrypoint to the program - everything gets set up and
--     run from here. Most of what's going on is dictated by the Brick library,
--     including the "App" data type which underpins most of the program.
main :: IO ()
main = do
  -- Get the command line arguments to the program
  args <- getArgs

  if "total" `elem` args
    then runTotal
    else runTUI ("ai" `elem` args)

--------------------------------------------------------------------------------
-- RUNNING THE AI DIRECTLY

getAverageDirect :: Int -> IO Rational
getAverageDirect n = do
  results <- replicateM n runAIDirectly
  pure $ fromIntegral (sum results) % fromIntegral n

-- Run the AI directly, without the TUI, and print the average score
runTotal :: IO ()
runTotal = do
  args <- getArgs
  putStrLn "Running the AI directly."
  -- find out how many to run by finding any number in the args
  let n = fromMaybe 1 $ asum (map readMaybe args)
  putStrLn $ "Running " <> show n <> " games, please wait..."

  avg <- getAverageDirect n
  let -- Print a ratio nicely as a multiple of 100
      avgStr :: String
      avgStr = printf "%.2f" (fromRational avg :: Double)

  putStrLn $ "Average score after " <> show n <> " games: " <> avgStr

--------------------------------------------------------------------------------
-- RUNNING THE TUI

-- |
--  This function is the main entrypoint for the TUI. It sets up the event
--  channel, the app, the initial state, and then runs the app.
runTUI :: Bool -> IO ()
runTUI ai = do
  eventChannel <- Brick.newBChan 10

  let app =
        Brick.App
          { appDraw = drawScreen,
            appChooseCursor = neverShowCursor,
            appHandleEvent = handleEvent,
            appStartEvent = do
              vty <- getVtyHandle
              let output = outputIface vty
              if supportsMode output Mouse
                then liftIO $ setMode output Mouse True
                else error "Your terminal does not support mouse events :("

              void $
                liftIO $
                  forkIO $
                    forever $ do
                      Brick.writeBChan eventChannel Tick
                      threadDelay 100000

              updateSelection,
            appAttrMap = const allAttributes
          }

  ts <- makeInitialTUIState ai

  let builder = mkVty defaultConfig
  vty <- builder

  _ <- customMainWithVty vty builder (Just eventChannel) app ts

  exitSuccess

allAttributes :: AttrMap
allAttributes =
  attrMap
    defAttr
    [ ("hearts", red `on` white),
      ("diamonds", red `on` white),
      ("clubs", black `on` white),
      ("spades", black `on` white),
      ("title", defAttr `withForeColor` magenta),
      ("blue", defAttr `withForeColor` blue `withStyle` bold),
      ("red", defAttr `withForeColor` red `withStyle` bold),
      ("def", defAttr),
      ("bold", defAttr `withStyle` bold),
      ("green", defAttr `withForeColor` green),
      ("blueBg", defAttr `withBackColor` blue),
      ("redBg", defAttr `withBackColor` red),
      ("grayBg", defAttr `withBackColor` brightBlack),
      ("gold", defAttr `withForeColor` yellow)
    ]

--------------------------------------------------------------------------------
-- EVENTS

-- | Brick works by handling events, which are passed to the handleEvent
-- function. That includes mouse and keyboard events, but also the "tick" event
-- we use to animate the screen.
handleEvent :: BrickEvent Name Tick -> GameUpdate
-- Quit the game with the 'ESC' key
handleEvent (Brick.VtyEvent (EvKey KEsc [])) = liftIO exitSuccess
-- Quit the game with Ctrl+C
handleEvent (Brick.VtyEvent (EvKey (KChar 'c') [MCtrl])) = liftIO exitSuccess
-- Restart the game with the 'R' key
handleEvent (Brick.VtyEvent (EvKey (KChar 'r') [])) = do
  ai <- gets isAIGame
  liftIO (makeInitialTUIState ai) >>= put
-- This runs every 1/10th of a second to update the screen
-- (e.g. so we can do animations)
handleEvent (Brick.AppEvent Tick) = tick
-- Scroll wheel controls the logging window
handleEvent (Brick.VtyEvent (EvMouseDown _ _ BScrollDown _)) =
  vScrollBy (viewportScroll LogViewport) 1
handleEvent (Brick.VtyEvent (EvMouseDown _ _ BScrollUp _)) =
  vScrollBy (viewportScroll LogViewport) (-1)
handleEvent (MouseDown LogViewport BScrollDown _ _) =
  vScrollBy (viewportScroll LogViewport) 1
handleEvent (MouseDown LogViewport BScrollUp _ _) =
  vScrollBy (viewportScroll LogViewport) (-1)
-- Click on a card to select or deselect it
handleEvent (MouseDown (RenderedCard ix) BLeft _ _) = selectCard ix >> click
-- Click on the discard or play buttons
handleEvent (MouseDown DiscardBtn BLeft _ _) = discardHand >> click
handleEvent (MouseDown PlayBtn BLeft _ _) = playHand >> click
handleEvent (MouseUp _ (Just BLeft) _) = modify $ \t -> t {clicked = False}
handleEvent _ = pure ()

-- Used by the human user only
selectCard :: Int -> GameUpdate
selectCard ix = do
  TUIState {..} <- get
  let selectAllowed =
        currentMode == PlayCards
          && waiting == 0
          && not isAIGame
          && not clicked
  when selectAllowed (doSelectCard ix)

doSelectCard :: Int -> GameUpdate
doSelectCard ix = do
  TUIState {..} <- get
  let card = hand gameState !! ix
  if
    | card `elem` selectedCards -> do
        modify $ \t -> t {selectedCards = List.delete card selectedCards}
        updateSelection
    | length selectedCards < 5 -> do
        modify $ \t -> t {selectedCards = card : selectedCards}
        updateSelection
    | otherwise -> pure ()

-- Fired when the player clicks the "Discard" button
discardHand :: GameUpdate
discardHand = do
  TUIState {..} <- get
  let GameState {..} = gameState
  let discardAllowed =
        currentMode == PlayCards
          && waiting == 0
          && not (null selectedCards)
          && discardsRemaining > 0
          && not isAIGame
          && not clicked
  when discardAllowed doDiscardHand

-- Used by both the human user and the AI
doDiscardHand :: GameUpdate
doDiscardHand = do
  modify $ \t ->
    t
      { cardsToDiscard = reverseSort $ selectedCards t,
        currentMode = WaitDiscard
      }
  modifyGameState $ \g -> g {discardsRemaining = discardsRemaining g - 1}
  sc <- gets selectedCards
  writeLog $ txtWrap $ "You discarded " <> Text.pack (printHand sc)

-- Fired when the player clicks the "Play" button
playHand :: GameUpdate
playHand = do
  TUIState {..} <- get
  let GameState {..} = gameState
  let playAllowed =
        currentMode == PlayCards
          && waiting == 0
          && not (null selectedCards)
          && handsRemaining > 0
          && not isAIGame
          && not clicked
  when playAllowed doPlayHand

-- Used by both the human user and the AI
doPlayHand :: GameUpdate
doPlayHand = do
  modify $ \t ->
    t
      { cardsToPlay = reverseSort $ selectedCards t,
        currentMode = WaitPlay
      }
  modifyGameState $ \g -> g {handsRemaining = handsRemaining g - 1}
  sc <- gets selectedCards
  writeLog $ txt $ "You played " <> Text.pack (printHand sc)

updateSelection :: GameUpdate
updateSelection = do
  TUIState {..} <- get

  tryGet (CW.bestHandType selectedCards) >>= \case
    Nothing -> do
      modify $ \s ->
        s
          { selectedMult = Nothing,
            selectedChips = Nothing,
            selectedHandType = Nothing
          }
    Just ht -> do
      let (chips, mult) = handTypeValues ht
      modify $ \s ->
        s
          { selectedMult = Just mult,
            selectedChips = Just chips,
            selectedHandType = Just ht
          }

--------------------------------------------------------------------------------
-- TICKS

-- The Tick function animates the game.
-- Each one of the options here will call one of the animation functions in the
-- next section.

-- Update any accumulating values.
tick :: GameUpdate
tick = do
  -- Scroll the viewport to the bottom if we just logged a line of text
  gets justLogged >>= \j ->
    when j $ do
      vScrollToEnd (viewportScroll LogViewport)
      modify $ \ts -> ts {justLogged = False}

  -- If waiting is 0, we go ahead and perform an action.
  gets waiting >>= \case
    0 -> do
      gets currentMode >>= \case
        WaitDraw -> drawOneCard
        WaitDiscard -> discardOneCard
        WaitPlay -> playOneCard
        WaitScore -> scoreOneCard
        WaitUnscore -> unscoreOneCard
        WaitEmptyPlayArea -> emptyOneCard
        PlayCards -> do
          aig <- gets isAIGame
          when aig makeAIMove
        Initial -> drawCardsTUI
        GameOver -> pure ()

    -- If waiting is above 0, we decrement it (this is so we can use multiple ticks to pause between actions)
    n -> do
      modify $ \ts -> ts {waiting = n - 1}

{-
  The most important animation flow is after you play a card, which will go through the following in order:

  PlayCards (await next action from player)
  WaitPlay (move the cards one by one to the play area)
  WaitScore (add chips for each scoring card, left to right)
  WaitUnscore (hide chips again, left to right)
  WaitEmptyPlayArea (move the cards out of the play area)
  WaitDraw (draw cards to replace the ones that were played)
  PlayCards, or GameOver if out of hands

-}

--------------------------------------------------------------------------------
-- Changes to the TUI that happen frame by frame.
-- Most of these check some current variant of the game state and then perform
-- an action, or move to the next state.

drawOneCard :: GameUpdate
drawOneCard = do
  gets cardsToDraw >>= \case
    -- We shouldn't get here, but let's handle the case just on the offchance
    [] -> modify $ \ts -> ts {currentMode = PlayCards}
    [_] -> do
      modify $ \ts -> ts {currentMode = PlayCards, cardsToDraw = []}
      writeLog $ txt " " <+> hLimit 15 hBorder
    (_ : t) -> modify $ \ts -> ts {cardsToDraw = t}

discardOneCard :: GameUpdate
discardOneCard = do
  gets cardsToDiscard >>= \case
    [] -> do
      disc <- gets cardsDiscarded
      modify $ \ts -> ts {cardsDiscarded = []}
      modifyGameState $ \gs -> gs {hand = hand gs List.\\ disc}
      drawCardsTUI
    (c : cs) -> do
      modify $ \ts ->
        ts
          { cardsToDiscard = cs,
            cardsDiscarded = c : cardsDiscarded ts,
            numDiscards = numDiscards ts + 1
          }

playOneCard :: GameUpdate
playOneCard = do
  gets cardsToPlay >>= \case
    [] -> do
      cpa <- gets cardsInPlayArea
      tryGet (CW.whichCardsScore cpa) >>= \case
        Nothing -> modify $ \ts ->
          ts
            { currentMode = WaitScore,
              waiting = 3,
              cardsToScore = reverseSort []
            }
        Just scs -> modify $ \ts ->
          ts
            { currentMode = WaitScore,
              waiting = 10,
              cardsToScore = reverseSort scs
            }

      gets selectedHandType >>= \case
        Nothing ->
          writeError "Couldn't figure out hand type - have you completed Step 3?"
        Just ht -> do
          sc <- gets selectedChips
          sm <- gets selectedMult
          writeLog $
            hBox
              [ txt "The base score for this hand (",
                str $ show ht,
                txt ") is ",
                withAttr "gold" $ str $ case (sc, sm) of
                  (Just c, Just m) -> show $ c * m
                  _ -> "???",
                txt " (",
                withAttr "blue" $ str $ case sc of
                  Just c -> show c
                  _ -> "???",
                txt " chips x ",
                withAttr "red" $ str $ case sm of
                  Just m -> show m
                  _ -> "???",
                txt " mult)"
              ]
    (c : cc) -> do
      modify $ \ts ->
        ts
          { cardsToPlay = cc,
            cardsInPlayArea = cardsInPlayArea ts ++ [c],
            numCardsPlayed = numCardsPlayed ts + 1,
            currentMode = WaitPlay,
            waiting = 1
          }
      modifyGameState $ \g -> g {hand = List.delete c (hand g)}

scoreOneCard :: GameUpdate
scoreOneCard = do
  scored <- gets cardsScored
  gets cardsToScore >>= \case
    [] -> do
      modify $ \ts ->
        ts
          { currentMode = WaitUnscore,
            waiting = 10,
            cardsScored = reverse scored
          }
      tryGet (CW.scoreHand scored) >>= \case
        Nothing -> pure ()
        Just s -> do
          modifyGameState $ \g -> g {roundScore = roundScore g + s}
          writeLog $
            hBox
              [ txt "Total score for the hand: ",
                withAttr "gold" $ txt $ Text.pack $ show s
              ]
    (c : _) -> do
      modify $ \ts ->
        ts
          { cardsScored = c : scored,
            cardsToScore = List.delete c (cardsToScore ts),
            selectedChips = (+ rankScore (rank c)) <$> selectedChips ts,
            currentMode = WaitScore,
            waiting = 3
          }

unscoreOneCard :: GameUpdate
unscoreOneCard = do
  gets cardsScored >>= \case
    [] -> do
      modify $ \ts -> ts {currentMode = WaitEmptyPlayArea}
    (_ : cs) -> do
      modify $ \ts -> ts {cardsScored = cs}

emptyOneCard :: GameUpdate
emptyOneCard = do
  gets cardsInPlayArea >>= \case
    [] -> do
      modify $ \ts -> ts {selectedCards = []}
      drawCardsTUI
    (_ : cs) -> do
      modify $ \ts -> ts {cardsInPlayArea = cs}

makeAIMove :: GameUpdate
makeAIMove = do
  -- Generate a next move, if we don't have one
  aim <- gets aiMove
  case aim of
    Nothing -> do
      GameState {..} <- gets gameState
      tryGet (CW.myAI pastMoves hand) >>= \case
        Nothing -> do
          writeLog $ withAttr "red" $ txt "An error was thrown while trying to make an AI move. Giving up."
        Just mv -> do
          modify $ \ts -> ts {aiMove = Just mv}
    Just _ -> pure ()

  -- If the AI move exists, then we can execute it (one step at a time)
  scs <- gets selectedCards
  gets aiMove >>= \case
    Nothing -> pure ()
    Just (Move ty sel) -> case sel List.\\ scs of
      [] -> do
        case ty of
          Play -> do
            hr <- gets (handsRemaining . gameState)
            if hr <= 0
              then do
                writeError "Cannot play hand with 0 hands remaining. Giving up."
                modify $ \ts -> ts {aiMove = Nothing, currentMode = GameOver}
              else doPlayHand
          Discard -> do
            dr <- gets (discardsRemaining . gameState)
            if dr <= 0
              then do
                writeError "Cannot discard hand with 0 discards remaining. Giving up."
                modify $ \ts -> ts {aiMove = Nothing, currentMode = GameOver}
              else doDiscardHand
        modify $ \ts -> ts {aiMove = Nothing}
        modifyGameState $ \g -> g {pastMoves = pastMoves g ++ [Move ty sel]}
      (c : _) -> do
        modify $ \ts ->
          ts
            { selectedCards = c : selectedCards ts,
              waiting = 1
            }
        updateSelection

-- Draw some cards from the deck.
drawCardsTUI :: GameUpdate
drawCardsTUI = do
  updateSelection
  hr <- gets (handsRemaining . gameState)
  if hr == 0
    then do
      writeLog $ txt " -------------"
      writeLog $ txt "Game over!"
      rs <- gets (roundScore . gameState)
      writeLog $
        hBox
          [ txt " Your final score is ",
            withAttr "gold" $ txt $ Text.pack $ show rs
          ]
      writeLog $ txt "Press R to restart or ESCAPE to quit."
      modify $ \ts -> ts {currentMode = GameOver}
      pure ()
    else do
      h <- gets (hand . gameState)
      d <- gets (deck . gameState)
      ctd <- gets cardsToDraw

      -- writeError $ Text.pack $ show h
      let (newHand, newDeck) = splitAt (handSize - length h) d

      modify $ \ts ->
        ts
          { cardsToDraw = List.sort $ newHand ++ ctd,
            currentMode = WaitDraw,
            selectedCards = [],
            cardsDiscarded = []
          }
      modifyGameState $ \g ->
        g
          { hand = reverseSort $ hand g ++ newHand,
            deck = newDeck
          }

      writeLog $ txtWrap $ "You drew " <> Text.pack (printHand newHand)

{-# LANGUAGE OverloadedStrings #-}

{- |
  This module contains the code for the terminal user interface (TUI) for
  Halatro. You do NOT need to worry about the contents of this file, but it
  might be of interest to those of you looking to go deeper into advanced
  Haskell programming.

  Your code should go in src/CourseworkOne.hs.
-}
module Halatro.Frontend.Widgets where

import Brick hiding (Down)
import Brick.Widgets.Border
import Brick.Widgets.Center
import CourseworkOne qualified as CW
import Data.Bool
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Halatro.BasicGame
import Halatro.Constants
import Halatro.Frontend.Types
import Halatro.Frontend.Utils
import Halatro.Types
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- WIDGETS

{- |
  This is the function that takes the current game state and gives back a
  tree of widgets that should be rendered to the screen.

  It works a bit like Hatch (a function from current frame number to an Image),
  but with a much larger family of primitives and operations.
-}
drawScreen :: TUIState -> [Widget Name]
drawScreen (TUIState{..}) =
  pure $
    joinBorders $
      border $
        vBox
          [ title
          , hBorder
          , hBox
              [ hLimit 50 $
                  vBox
                    [ scoreRow
                    , cardsMoved
                    , fill ' '
                    , handsAndDiscards
                    ]
              , vBorder
              , vBox
                  [ hLimit 2 $ fill ' '
                  , playArea
                  , fill ' '
                  , renderHand
                  , fill ' '
                  , playAndDiscardBtns
                  ]
              ]
          , hLimit 3 hBorder <+> txt " Log " <+> hBorder
          , logWidget
          ]
 where
  GameState{..} = gameState

  box boxTitle value attr =
    vLimit 1 $
      hLimit 15 (hCenter $ txt boxTitle) <+> vBorder <+> hCenter (withAttr attr $ txt $ Text.pack value)

  scoreRow :: Widget Name
  scoreRow =
    let
     in joinBorders $
          vBox
            [ border $ box "Total Score" (show roundScore) "gold"
            , border $
                vBox
                  [ hCenter $ txt "Current Hand:"
                  , hBorder
                  , selectedHand
                  , hBorder
                  , box "Chips" (maybe "???" show selectedChips) "blue"
                  , hBorder
                  , box "Multiplier" (maybe "???" show selectedMult) "red"
                  ]
            ]

  cardsMoved =
    joinBorders $
      border $
        vBox
          [ hCenter $ str $ "Cards discarded: " <> show numDiscards
          , hCenter $ str $ "Cards played: " <> show numCardsPlayed
          , hCenter $ str $ "Remaining in deck: " <> show (length deck + length cardsToDraw) <> "/52"
          ]

  handsAndDiscards :: Widget Name
  handsAndDiscards =
    joinBorders $
      hBox
        [ border $ box "Hands left" (show handsRemaining) "blue"
        , border $ box "Discards left" (show discardsRemaining) "red"
        ]

  playArea :: Widget Name
  playArea =
    let
      wcs = fromMaybe [] $ unsafePerformIO $ tryGetIO (CW.whichCardsScore cardsInPlayArea)
      willScore c =
        c `elem` wcs && case currentMode of
          WaitScore -> True
          _ -> False
      genSelCard ix c =
        padTop Max $
          renderCard ix c False True (c `elem` cardsScored) (willScore c)
      cardSels = zipWith genSelCard [0 ..] cardsInPlayArea
     in
      vLimit 8 $
        hBox
          [ hLimit 1 $ fill ' '
          , hCenter (hBox $ List.intersperse (txt "   ") cardSels)
          , hLimit 1 $ fill ' '
          ]

  renderHand :: Widget Name
  renderHand =
    let
      cardSels = zipWith (\ix c -> renderCard ix c (c `elem` selectedCards) (c `notElem` cardsToDraw && c `notElem` cardsDiscarded) False False) [0 ..] hand
     in
      hBox
        [ vLimit 7 $ hLimit 1 $ fill ' '
        , hCenter (hBox cardSels) <=> hCenter (txt "Your hand")
        , vLimit 7 $ hLimit 1 $ fill ' '
        ]

  selectedHand :: Widget Name
  selectedHand = case selectedHandType of
    Nothing ->
      withAttr "red" $
        vBox
          [ hCenter $ txt "Can't determine hand type"
          , hCenter $ txt "(Have you completed Step 3?)"
          ]
    Just ht ->
      vLimit 1 $ hCenter $ txt "Selected hand type: " <+> str (show ht)

  playAndDiscardBtns :: Widget Name
  playAndDiscardBtns =
    let
      playBtn =
        clickable PlayBtn $
          forceAttr "blueBg" $
            padAll 1 $
              hCenter $
                txt "Play"
      discardBtn =
        clickable DiscardBtn $
          forceAttr (if discardsRemaining > 0 then "redBg" else "grayBg") $
            padAll 1 $
              hCenter $
                txt "Discard"
     in
      hCenter $ hBox [txt " ", playBtn, txt "  ", discardBtn, txt " "]

  logWidget =
    vLimit 10
      $ withVScrollBars OnRight
      $ viewport
        LogViewport
        Vertical
      $ vBox logLines

title :: Widget Name
title =
  withAttr "title" $
    hCenter $
      txt $
        Text.unlines
          [ "██╗  ██╗ █████╗ ███╗     █████╗ ████████╗██████╗  ██████╗ "
          , "██║  ██║██╔══██╗ ███║   ██╔══██╗╚══██╔══╝██╔══██╗██╔═══██╗"
          , "███████║███████║  ███║  ███████║   ██║   ██████╔╝██║   ██║"
          , "██╔══██║██╔══██║ ██║██║ ██╔══██║   ██║   ██╔══██╗██║   ██║"
          , "██║  ██║██║  ██║██║  ██║██║  ██║   ██║   ██║  ██║╚██████╔╝"
          , "╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝ ╚═════╝ "
          ]

{- | Render a single card as a widget.
     The specifics of how it will be rendered will differ depending on where in
     the game it is (which is why there are so many arguments being passed).
-}
renderCard :: Int -> Card -> Bool -> Bool -> Bool -> Bool -> Widget Name
renderCard ix (Card r s) isSelected doShow isScored willScore
  | doShow =
      clickable
        (RenderedCard ix)
        $ withSelColor
        $ vBox
          [ if isScored
              then hLimit 7 $ withAttr "blue" $ hCenter $ txt $ "+" <> Text.justifyRight 2 ' ' (Text.pack $ show $ rankScore r)
              else emptyWidget
          , bool (txt " ") emptyWidget (isSelected && not willScore)
          , border $ withColor $ txt $ Text.unlines [top, middle, bottom]
          , bool emptyWidget (txt " ") isSelected
          , bool emptyWidget (txt " ") willScore
          ]
  | otherwise = emptyWidget
 where
  withSelColor = if isSelected then withAttr "green" else id

  withColor = withAttr $ case s of
    Hearts -> "hearts"
    Diamonds -> "diamonds"
    Clubs -> "clubs"
    Spades -> "spades"

  top = Text.justifyLeft 5 ' ' $ Text.pack $ " " <> shortRank r
  middle = Text.center 5 ' ' $ Text.pack $ shortSuit s <> " " <> shortSuit s
  bottom = Text.justifyRight 5 ' ' $ Text.pack $ shortRank r <> " "

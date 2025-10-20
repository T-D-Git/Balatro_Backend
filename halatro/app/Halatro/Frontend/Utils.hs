{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
  This module contains the code for the terminal user interface (TUI) for
  Halatro. You do NOT need to worry about the contents of this file, but it
  might be of interest to those of you looking to go deeper into advanced
  Haskell programming.

  Your code should go in src/CourseworkOne.hs.
-}
module Halatro.Frontend.Utils where

import Brick hiding (Down)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad.IO.Class (liftIO)
import Data.List qualified as List
import Data.Ord
import Data.String (IsString (fromString))
import Data.Text (Text)
import Halatro.Frontend.Types

--------------------------------------------------------------------------------
-- UTILITY FUNCTIONS

reverseSort :: (Ord a) => [a] -> [a]
reverseSort = List.sortBy (comparing Down)

tryGet :: a -> EventM Name TUIState (Maybe a)
tryGet a = liftIO (tryGetIO a)

tryGetIO :: a -> IO (Maybe a)
tryGetIO a =
  fmap Just (evaluate a) `catch` \(_ :: SomeException) -> pure Nothing

writeLog :: Widget Name -> EventM Name TUIState ()
writeLog line = modify $ \t ->
  t
    { logLines = logLines t ++ [line]
    , justLogged = True
    }

writeError :: Text -> EventM Name TUIState ()
writeError = do writeLog . withAttr "red" . txt

click :: EventM Name TUIState ()
click = modify $ \t -> t{clicked = True}

instance IsString AttrName where
  fromString :: String -> AttrName
  fromString = attrName

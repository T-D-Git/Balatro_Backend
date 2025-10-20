{- |
  This module contains the code for the terminal user interface (TUI) for
  Halatro. You do NOT need to worry about the contents of this file, but it
  might be of interest to those of you looking to go deeper into advanced
  Haskell programming.

  Your code should go in src/CourseworkOne.hs.
-}
module Main (main) where

import Halatro.Frontend qualified as TUI

main :: IO ()
main = TUI.main

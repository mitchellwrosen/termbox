{-# LANGUAGE PatternSynonyms #-}

-- |
-- A @termbox@ program is typically constructed as an infinite loop that:
--
-- 1. Renders a scene.
-- 2. Polls for an event.
--
-- For example, this progam simply displays the number of keys pressed, and
-- quits on @Esc@:
--
-- @
-- {-\# LANGUAGE LambdaCase \#-}
--
-- import qualified Termbox
--
-- main :: IO ()
-- main =
--   Termbox.'run' (\\_width _height render poll -> loop render poll 0)
--
-- loop :: (Termbox.'Cells' -> Termbox.'Cursor' -> IO ()) -> IO Termbox.'Event' -> Int -> IO ()
-- loop render poll n = do
--   render (string (show n)) Termbox.'NoCursor'
--
--   poll >>= \\case
--     Termbox.'EventKey' Termbox.'KeyEsc' -> pure ()
--     _ -> loop render poll (n+1)
--
-- string :: Int -> Int -> String -> Termbox.'Cells'
-- string col row =
--   foldMap (\\(i, c) -> Termbox.'set' (col + i) row (Termbox.'Cell' c 0 0)) . zip [0..]
-- @
--
-- Other termbox features include cell attributes (style, color), cursor
-- display, and mouse click handling.
--
-- This module is intended to be imported qualified.
module Termbox
  ( -- * Initialization
    run,
    InitError (..),

    -- * Terminal contents
    set,
    Cells,
    Cell (..),
    Cursor (..),

    -- * Event handling
    Event (..),
    Key (..),
    -- $key-aliases
    pattern KeyCtrlH,
    pattern KeyCtrlLsqBracket,
    pattern KeyCtrl2,
    pattern KeyCtrl3,
    pattern KeyCtrl4,
    pattern KeyCtrl5,
    pattern KeyCtrl7,
    pattern KeyCtrlM,
    pattern KeyCtrlI,
    pattern KeyCtrlUnderscore,
    Mouse (..),
    PollError (..),

    -- * Attributes
    Attr,
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    bold,
    underline,
    reverse,
  )
where

import Control.Exception
import Data.Semigroup (Semigroup (..))
import Termbox.Attr
  ( Attr,
    black,
    blue,
    bold,
    cyan,
    green,
    magenta,
    red,
    reverse,
    underline,
    white,
    yellow,
  )
import Termbox.Cell (Cell (Cell))
import Termbox.Cells (Cells (Cells), set)
import Termbox.Event (Event (..), PollError (..), poll)
import Termbox.Internal
import Termbox.Key
  ( Key (..),
    pattern KeyCtrl2,
    pattern KeyCtrl3,
    pattern KeyCtrl4,
    pattern KeyCtrl5,
    pattern KeyCtrl7,
    pattern KeyCtrlH,
    pattern KeyCtrlI,
    pattern KeyCtrlLsqBracket,
    pattern KeyCtrlM,
    pattern KeyCtrlUnderscore,
  )
import Termbox.Mouse (Mouse (..))
import Prelude hiding (reverse)

-- | A cursor.
data Cursor
  = -- | Column, then row
    Cursor !Int !Int
  | NoCursor

-- $key-aliases
-- In a few cases, distinct key sequences map to equivalent key events. The pattern synonyms below are provided for an
-- alternate syntax in these cases, if desired.

-- | Termbox initialization errors.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving (Show)

instance Exception InitError

-- | Run a @termbox@ program and restore the terminal state afterwards.
--
-- The function provided to @run@ is provided:
--
--   * The initial terminal width
--   * The initial terminal height
--   * An action that renders a scene
--   * An action that polls for an event indefinitely
--
-- /Throws/: 'InitError'
run :: (Int -> Int -> (Cells -> Cursor -> IO ()) -> IO Event -> IO a) -> IO a
run action = do
  mask $ \unmask -> do
    initResult <- tb_init
    case () of
      _ | initResult == 0 -> do
        result <-
          unmask
            ( do
                _ <- tb_select_input_mode tB_INPUT_MOUSE
                _ <- tb_select_output_mode tB_OUTPUT_256
                width <- tb_width
                height <- tb_height
                action width height render poll
            )
            `onException` shutdown
        shutdown
        pure result
      _ | initResult == tB_EFAILED_TO_OPEN_TTY -> throwIO FailedToOpenTTY
      _ | initResult == tB_EPIPE_TRAP_ERROR -> throwIO PipeTrapError
      _ | initResult == tB_EUNSUPPORTED_TERMINAL -> throwIO UnsupportedTerminal
      _ -> error ("termbox: unknown tb_init error " ++ show initResult)

-- | Render a scene.
render :: Cells -> Cursor -> IO ()
render (Cells cells) cursor = do
  tb_set_clear_attributes 0 0
  tb_clear
  cells
  case cursor of
    Cursor col row -> tb_set_cursor col row
    NoCursor -> tb_set_cursor tB_HIDE_CURSOR tB_HIDE_CURSOR
  tb_present

shutdown :: IO ()
shutdown = do
  _ <- tb_select_output_mode tB_OUTPUT_NORMAL
  tb_shutdown

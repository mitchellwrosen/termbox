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
--   Termbox.'run_' (loop 0)
--
-- loop :: Int -> IO ()
-- loop n = do
--   Termbox.'render' (string (show n)) Termbox.'NoCursor'
--
--   Termbox.'poll' >>= \\case
--     Termbox.'EventKey' Termbox.'KeyEsc' -> pure ()
--     _ -> loop (n+1)
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
    run_,
    InitError (..),

    -- * Terminal contents
    set,
    render,
    getCells,
    getSize,
    Cells,
    Cell (..),
    Cursor (..),

    -- * Event handling
    poll,
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
    Attr,
  )
where

import Control.Exception
import Control.Monad ((>=>))
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
import Termbox.Cell (Cell (Cell), getCells)
import Termbox.Cells (Cells (Cells), set)
import Termbox.Cursor (Cursor (Cursor, NoCursor))
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
run :: IO a -> IO (Either InitError a)
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
                action
            )
            `onException` shutdown
        shutdown
        pure (Right result)
      _ | initResult == tB_EFAILED_TO_OPEN_TTY -> pure (Left FailedToOpenTTY)
      _ | initResult == tB_EPIPE_TRAP_ERROR -> pure (Left PipeTrapError)
      _ | initResult == tB_EUNSUPPORTED_TERMINAL -> pure (Left UnsupportedTerminal)
      _ -> error ("termbox: unknown tb_init error " ++ show initResult)
  where
    shutdown :: IO ()
    shutdown = do
      _ <- tb_select_output_mode tB_OUTPUT_NORMAL
      tb_shutdown

-- | Like 'run', but throws 'InitError's as @IO@ exceptions.
--
-- /Throws/: 'InitError'
run_ :: IO a -> IO a
run_ =
  run >=> either throwIO pure

-- | Get the terminal size (width, then height).
getSize :: IO (Int, Int)
getSize =
  (,) <$> tb_width <*> tb_height

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

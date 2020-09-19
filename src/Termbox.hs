-- |
-- A @termbox@ program is typically constructed as an infinite loop that:
--
-- 1. 'clear's the terminal backbuffer.
-- 2. Renders the program state by 'set'ting individual pixels.
-- 3. 'flush'es the backbuffer to the terminal.
-- 4. 'poll's for an event to update the program state.
--
-- For example, this progam simply displays the number of keys pressed, and
-- quits on @Esc@:
--
-- @
-- {-\# LANGUAGE LambdaCase \#-}
--
-- import Data.Foldable (for_)
-- import qualified Termbox
--
-- main :: IO ()
-- main =
--   Termbox.'run_' (loop 0)
--
-- loop :: Int -> IO ()
-- loop n = do
--   Termbox.'clear' mempty mempty
--   render n
--   Termbox.'flush'
--
--   Termbox.'poll' >>= \\case
--     Termbox.'EventKey' Termbox.'KeyEsc' _ -> pure ()
--     _ -> loop (n+1)
--
-- render :: Int -> IO ()
-- render n =
--   for_
--     (zip [0..] (show n))
--     (\\(i, c) -> Termbox.'set' i 0 (Termbox.'Cell' c mempty mempty))
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
    getCells,
    clear,
    flush,
    Cell (..),

    -- * Terminal size
    getSize,

    -- * Cursor manipulation
    setCursor,
    hideCursor,

    -- * Event handling
    poll,
    Event (..),
    Key (..),
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

    -- * Terminal modes
    getInputMode,
    setInputMode,
    InputMode (..),
    MouseMode (..),
    getOutputMode,
    setOutputMode,
    OutputMode (..),
  )
where

import Control.Exception
import Control.Monad ((>=>))
import Data.Semigroup (Semigroup (..))
import Termbox.Attr
  ( Attr,
    attrToWord,
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
import qualified Termbox.C
import Termbox.Cell (Cell (..), getCells, set)
import Termbox.Cursor (hideCursor, setCursor)
import Termbox.Event (Event (..), PollError (..), poll)
import Termbox.InputMode (InputMode (..), getInputMode, setInputMode)
import Termbox.Key (Key (..))
import Termbox.Mouse (Mouse (..))
import Termbox.MouseMode (MouseMode (..))
import Termbox.OutputMode (OutputMode (..), getOutputMode, setOutputMode)
import Prelude hiding (reverse)

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Termbox initialization errors that can be returned by 'run'.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving (Show)

instance Exception InitError

-- | Run a @termbox@ program and restore the terminal state afterwards.
run :: IO a -> IO (Either InitError a)
run action =
  mask $ \unmask ->
    Termbox.C.init >>= \case
      Termbox.C.InitOk -> do
        result <- unmask action `onException` Termbox.C.shutdown
        Termbox.C.shutdown
        pure (Right result)
      Termbox.C.FailedToOpenTTY -> pure (Left FailedToOpenTTY)
      Termbox.C.PipeTrapError -> pure (Left PipeTrapError)
      Termbox.C.UnsupportedTerminal -> pure (Left UnsupportedTerminal)

-- | Like 'run', but throws 'InitError's as @IO@ exceptions.
run_ :: IO a -> IO a
run_ =
  run >=> either throwIO pure

-- | Get the terminal size (width, then height).
getSize :: IO (Int, Int)
getSize =
  (,) <$> Termbox.C.width <*> Termbox.C.height

-- | Clear the back buffer with the given foreground and background attributes.
clear :: Attr -> Attr -> IO ()
clear fg bg = do
  Termbox.C.setClearAttributes (attrToWord fg) (attrToWord bg)
  Termbox.C.clear

-- | Synchronize the internal back buffer with the terminal.
flush :: IO ()
flush =
  Termbox.C.present

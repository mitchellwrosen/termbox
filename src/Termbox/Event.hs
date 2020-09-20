{-# LANGUAGE TypeApplications #-}

module Termbox.Event
  ( Event (..),
    poll,
    PollError (..),
  )
where

import Control.Exception (Exception, throwIO)
import Data.Char (chr)
import Data.Int (Int32)
import Data.Semigroup (Semigroup (..))
import Data.Word (Word32)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Termbox.Internal
import Termbox.Key (Key (KeyChar), parseKey)
import Termbox.Mouse (Mouse, parseMouse)
import Prelude hiding (mod)

-- | A input event.
data Event
  = -- | Key event. The bool indicates the /alt/ modifier.
    EventKey !Key !Bool
  | -- | Resize event (width, then height)
    EventResize !Int !Int
  | -- | Mouse event (column, then row)
    EventMouse !Mouse !Int !Int
  deriving (Eq, Show)

-- | Block until an 'Event' arrives.
--
-- /Note/: @termbox v1.1.2@ does not properly handle OS signals that interrupt
-- the underlying @select@ system call, so unfortunately the familiar @Ctrl-C@
-- will not be able to stop a program stuck in 'pollEvent'.
--
-- You can work around this issue by polling in a background thread using the
-- @threaded@ runtime, or simply writing event-handling code that is responsive
-- to intuitive "quit" keys like @q@ and @Esc@.
--
-- /Throws/: 'PollError'
poll :: IO Event
poll =
  alloca $ \ptr ->
    tb_poll_event ptr >>= \case
      -1 -> throwIO PollError
      _ -> parseEvent <$> peek ptr

-- | An error occurred when 'poll'ing, due to mysterious circumstances that are
-- not well-documented in the original C codebase.
data PollError
  = PollError
  deriving (Show)

instance Exception PollError

-- | Parse an 'Event' from a 'TbEvent'.
parseEvent :: TbEvent -> Event
parseEvent = \case
  TbEvent typ mod key ch w h x y ->
    if typ == tB_EVENT_KEY
      then
        EventKey
          ( case ch of
              0 -> parseKey key
              _ -> KeyChar (chr (fromIntegral @Word32 @Int ch))
          )
          ( case () of
              _ | mod == 0 -> False
              _ | mod == tB_MOD_ALT -> True
              _ -> error ("termbox: unknown key modifier " ++ show mod)
          )
      else
        if typ == tB_EVENT_RESIZE
          then EventResize (fromIntegral @Int32 @Int w) (fromIntegral @Int32 @Int h)
          else
            if typ == tB_EVENT_MOUSE
              then EventMouse (parseMouse key) (fromIntegral @Int32 @Int x) (fromIntegral @Int32 @Int y)
              else error ("termbox: unknown event type " ++ show typ)

module Termbox.Event
  ( Event (..),
    poll,
    PollError (..),
  )
where

import Control.Exception (Exception, throwIO)
import Data.Semigroup (Semigroup (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Termbox.C
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
  TbEvent TbEventTypeKey mod key ch _ _ _ _ ->
    EventKey
      ( case ch of
          '\0' -> parseKey key
          _ -> KeyChar ch
      )
      ( case mod of
          TbModAlt -> True
          _ -> False
      )
  TbEvent TbEventTypeResize _ _ _ w h _ _ -> EventResize w h
  TbEvent TbEventTypeMouse _ key _ _ _ x y -> EventMouse (parseMouse key) x y

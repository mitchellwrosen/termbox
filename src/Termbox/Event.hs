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
  = -- | Key event.
    EventKey !Key
  | -- | Resize event (width, then height)
    EventResize !Int !Int
  | -- | Mouse event (column, then row)
    EventMouse !Mouse !Int !Int
  deriving (Eq, Show)

-- | Block until an 'Event' arrives.
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
parseEvent (TbEvent typ _mod key ch w h x y)
  | typ == tB_EVENT_KEY =
    EventKey (if ch == 0 then parseKey key else KeyChar (chr (fromIntegral @Word32 @Int ch)))
  | typ == tB_EVENT_RESIZE = EventResize (fromIntegral @Int32 @Int w) (fromIntegral @Int32 @Int h)
  | typ == tB_EVENT_MOUSE = EventMouse (parseMouse key) (fromIntegral @Int32 @Int x) (fromIntegral @Int32 @Int y)
  | otherwise = error ("termbox: unknown event type " ++ show typ)

module Termbox.MouseMode
  ( MouseMode (..),
  )
where

-- | The mouse mode.
--
-- * __No__. Don't handle mouse events.
--
-- * __Yes__. Handle mouse events.
data MouseMode
  = -- | Default.
    MouseModeNo
  | MouseModeYes
  deriving (Eq, Ord, Show)

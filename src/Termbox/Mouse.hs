module Termbox.Mouse
  ( Mouse (..),
    parseMouse,
  )
where

import qualified Termbox.C

-- | A mouse event.
data Mouse
  = MouseLeft
  | MouseMiddle
  | MouseRelease
  | MouseRight
  | MouseWheelDown
  | MouseWheelUp
  deriving (Eq, Ord, Show)

-- | Parse a 'Mouse' from a 'Termbox.C.Key'.
parseMouse :: Termbox.C.Key -> Mouse
parseMouse = \case
  Termbox.C.KeyMouseLeft -> MouseLeft
  Termbox.C.KeyMouseMiddle -> MouseMiddle
  Termbox.C.KeyMouseRelease -> MouseRelease
  Termbox.C.KeyMouseRight -> MouseRight
  Termbox.C.KeyMouseWheelDown -> MouseWheelDown
  Termbox.C.KeyMouseWheelUp -> MouseWheelUp
  key -> error ("termbox: unknown mouse " ++ show key)

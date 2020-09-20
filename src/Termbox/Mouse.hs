module Termbox.Mouse
  ( Mouse (..),
    parseMouse,
  )
where

import Termbox.C (TbKey (..))

-- | A mouse event.
data Mouse
  = MouseLeft
  | MouseMiddle
  | MouseRelease
  | MouseRight
  | MouseWheelDown
  | MouseWheelUp
  deriving (Eq, Ord, Show)

-- | Parse a 'Mouse' from a 'TbKey'.
parseMouse :: TbKey -> Mouse
parseMouse = \case
  TbKeyMouseLeft -> MouseLeft
  TbKeyMouseMiddle -> MouseMiddle
  TbKeyMouseRelease -> MouseRelease
  TbKeyMouseRight -> MouseRight
  TbKeyMouseWheelDown -> MouseWheelDown
  TbKeyMouseWheelUp -> MouseWheelUp
  key -> error ("termbox: unknown mouse " ++ show key)

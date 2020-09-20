module Termbox.Mouse
  ( Mouse (..),
    parseMouse,
  )
where

import Data.Word (Word16)
import Termbox.Internal

-- | A mouse event.
data Mouse
  = MouseLeft
  | MouseMiddle
  | MouseRelease
  | MouseRight
  | MouseWheelDown
  | MouseWheelUp
  deriving (Eq, Ord, Show)

parseMouse :: Word16 -> Mouse
parseMouse key
  | key == tB_KEY_MOUSE_LEFT = MouseLeft
  | key == tB_KEY_MOUSE_MIDDLE = MouseMiddle
  | key == tB_KEY_MOUSE_RELEASE = MouseRelease
  | key == tB_KEY_MOUSE_RIGHT = MouseRight
  | key == tB_KEY_MOUSE_WHEEL_DOWN = MouseWheelDown
  | key == tB_KEY_MOUSE_WHEEL_UP = MouseWheelUp
  | otherwise = error ("termbox: unknown mouse " ++ show key)

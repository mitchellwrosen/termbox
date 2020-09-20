module Termbox.InputMode
  ( InputMode (..),
    defaultInputMode,
    setInputMode,
  )
where

import Data.Bits ((.|.))
import Data.Functor (void)
import Termbox.Internal
import Termbox.MouseMode (MouseMode (..))

-- | The input modes.
--
-- * __Esc__. When ESC sequence is in the buffer and it doesn't match any known
-- sequence, ESC means 'KeyEsc'.
--
-- * __Alt__. When ESC sequence is in the buffer and it doesn't match any known
-- sequence, ESC enables the /alt/ modifier for the next keyboard event.
data InputMode
  = -- | Default.
    InputModeEsc MouseMode
  | InputModeAlt MouseMode
  deriving (Eq, Ord, Show)

defaultInputMode :: InputMode
defaultInputMode =
  InputModeEsc MouseModeNo

-- | Set the input mode.
setInputMode :: InputMode -> IO ()
setInputMode =
  void . tb_select_input_mode . f
  where
    f :: InputMode -> Int
    f = \case
      InputModeEsc MouseModeNo -> tB_INPUT_ESC
      InputModeEsc MouseModeYes -> tB_INPUT_ESC .|. tB_INPUT_MOUSE
      InputModeAlt MouseModeNo -> tB_INPUT_ALT
      InputModeAlt MouseModeYes -> tB_INPUT_ALT .|. tB_INPUT_MOUSE

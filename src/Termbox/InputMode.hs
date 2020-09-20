module Termbox.InputMode
  ( InputMode (..),
    defaultInputMode,
    getInputMode,
    setInputMode,
  )
where

import Data.Bits ((.|.))
import Data.Functor (void)
import Termbox.C
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

-- | Get the current input mode.
getInputMode :: IO InputMode
getInputMode =
  f <$> tb_select_input_mode tB_INPUT_CURRENT
  where
    f :: Int -> InputMode
    f = \case
      1 -> InputModeEsc MouseModeNo
      2 -> InputModeAlt MouseModeNo
      5 -> InputModeEsc MouseModeYes
      6 -> InputModeAlt MouseModeYes
      n -> error ("termbox: unknown input mode " ++ show n)

-- | Set the input mode.
setInputMode :: InputMode -> IO ()
setInputMode =
  void . tb_select_input_mode . f
  where
    f :: InputMode -> Int
    f = \case
      InputModeEsc MouseModeNo -> Termbox.C.tB_INPUT_ESC
      InputModeEsc MouseModeYes -> Termbox.C.tB_INPUT_ESC .|. Termbox.C.tB_INPUT_MOUSE
      InputModeAlt MouseModeNo -> Termbox.C.tB_INPUT_ALT
      InputModeAlt MouseModeYes -> Termbox.C.tB_INPUT_ALT .|. Termbox.C.tB_INPUT_MOUSE

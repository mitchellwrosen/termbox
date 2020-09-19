module Termbox.InputMode
  ( InputMode (..),
    getInputMode,
    setInputMode,
  )
where

import Data.Bits ((.|.))
import Data.Functor (void)
import qualified Termbox.C
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

-- | Get the current input mode.
getInputMode :: IO InputMode
getInputMode =
  f <$> Termbox.C.selectInputMode Termbox.C._INPUT_CURRENT
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
  void . Termbox.C.selectInputMode . f
  where
    f :: InputMode -> Int
    f = \case
      InputModeEsc MouseModeNo -> Termbox.C._INPUT_ESC
      InputModeEsc MouseModeYes -> Termbox.C._INPUT_ESC .|. Termbox.C._INPUT_MOUSE
      InputModeAlt MouseModeNo -> Termbox.C._INPUT_ALT
      InputModeAlt MouseModeYes -> Termbox.C._INPUT_ALT .|. Termbox.C._INPUT_MOUSE

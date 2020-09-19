module Termbox.OutputMode
  ( OutputMode (..),
    getOutputMode,
    setOutputMode,
  )
where

import Data.Functor (void)
import qualified Termbox.C

-- | The output modes.
--
-- * __Normal__. Supports colors /0..8/, which includes all named color
-- attributes exported by this library, e.g. 'red'.
--
-- * __Grayscale__. Supports colors /0..23/.
--
-- * __216__. Supports colors /0..216/.
--
-- * __256__. Supports colors /0..255/.
data OutputMode
  = -- | Default.
    OutputModeNormal
  | OutputModeGrayscale
  | OutputMode216
  | OutputMode256
  deriving (Eq, Ord, Show)

-- | Get the current output mode.
getOutputMode :: IO OutputMode
getOutputMode =
  f <$> Termbox.C.selectOutputMode Termbox.C.OutputModeCurrent
  where
    f :: Termbox.C.OutputMode -> OutputMode
    f = \case
      Termbox.C.OutputModeNormal -> OutputModeNormal
      Termbox.C.OutputMode256 -> OutputMode256
      Termbox.C.OutputMode216 -> OutputMode216
      Termbox.C.OutputModeGrayscale -> OutputModeGrayscale
      Termbox.C.OutputModeCurrent -> error "termbox: getOutputMode returned OutputModeCurrent"

-- | Set the output mode.
setOutputMode :: OutputMode -> IO ()
setOutputMode =
  void . Termbox.C.selectOutputMode . f
  where
    f :: OutputMode -> Termbox.C.OutputMode
    f = \case
      OutputModeNormal -> Termbox.C.OutputModeNormal
      OutputMode256 -> Termbox.C.OutputMode256
      OutputMode216 -> Termbox.C.OutputMode216
      OutputModeGrayscale -> Termbox.C.OutputModeGrayscale

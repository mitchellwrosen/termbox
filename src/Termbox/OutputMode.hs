module Termbox.OutputMode
  ( OutputMode (..),
    defaultOutputMode,
    setOutputMode,
  )
where

import Data.Functor (void)
import Termbox.Internal

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

defaultOutputMode :: OutputMode
defaultOutputMode =
  OutputModeNormal

-- | Set the output mode.
setOutputMode :: OutputMode -> IO ()
setOutputMode =
  void . tb_select_output_mode . \case
    OutputModeNormal -> tB_OUTPUT_NORMAL
    OutputMode256 -> tB_OUTPUT_256
    OutputMode216 -> tB_OUTPUT_216
    OutputModeGrayscale -> tB_OUTPUT_GRAYSCALE

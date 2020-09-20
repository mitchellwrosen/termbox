module Termbox.OutputMode
  ( OutputMode (..),
    getOutputMode,
    setOutputMode,
  )
where

import Data.Functor (void)
import Termbox.C

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
  f <$> tb_select_output_mode TbOutputModeCurrent
  where
    f :: TbOutputMode -> OutputMode
    f = \case
      TbOutputModeNormal -> OutputModeNormal
      TbOutputMode256 -> OutputMode256
      TbOutputMode216 -> OutputMode216
      TbOutputModeGrayscale -> OutputModeGrayscale
      TbOutputModeCurrent -> error "termbox: getOutputMode returned OutputModeCurrent"

-- | Set the output mode.
setOutputMode :: OutputMode -> IO ()
setOutputMode =
  void . tb_select_output_mode . f
  where
    f :: OutputMode -> TbOutputMode
    f = \case
      OutputModeNormal -> TbOutputModeNormal
      OutputMode256 -> TbOutputMode256
      OutputMode216 -> TbOutputMode216
      OutputModeGrayscale -> TbOutputModeGrayscale

module Termbox.Cursor
  ( hideCursor,
    setCursor,
  )
where

import qualified Termbox.C

-- | Set the cursor coordinates (column, then row).
setCursor :: Int -> Int -> IO ()
setCursor =
  Termbox.C.setCursor

-- | Hide the cursor.
hideCursor :: IO ()
hideCursor =
  Termbox.C.setCursor Termbox.C._HIDE_CURSOR Termbox.C._HIDE_CURSOR

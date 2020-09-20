module Termbox.Cursor
  ( hideCursor,
    setCursor,
  )
where

import Termbox.Internal

-- | Set the cursor coordinates (column, then row).
setCursor :: Int -> Int -> IO ()
setCursor =
  tb_set_cursor

-- | Hide the cursor.
hideCursor :: IO ()
hideCursor =
  tb_set_cursor tB_HIDE_CURSOR tB_HIDE_CURSOR

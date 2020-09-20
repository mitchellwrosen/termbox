module Termbox.Cells
  ( Cells (..),
    set,
  )
where

import Data.Char (ord)
import Termbox.Attr (attrToWord)
import Termbox.Cell (Cell (Cell))
import Termbox.Internal (tb_change_cell)

-- | A grid of cells.
--
-- Create a 'Cells' with 'set' or 'mempty' and combine them with ('<>').
newtype Cells
  = Cells (IO ())
  deriving {- newtype -} (Monoid, Semigroup)

-- | Set a single cell's value (column, then row).
set :: Int -> Int -> Cell -> Cells
set col row (Cell ch fg bg) =
  Cells (tb_change_cell col row (fromIntegral (ord ch)) (attrToWord fg) (attrToWord bg))

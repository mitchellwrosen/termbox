module Termbox.Cell
  ( Cell (..),
    set,
    getCells,
  )
where

import Control.Monad (join)
import Data.Array (Array)
import qualified Data.Array.Storable as Array (freeze)
import qualified Data.Array.Storable.Internals as Array
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import Foreign.Ptr (Ptr)
import Foreign.Storable
import Termbox.Attr (Attr, attrToWord, wordToAttr)
import qualified Termbox.C
import Termbox.C (tb_change_cell, tb_height, tb_width)

-- | A 'Cell' contains a character, foreground attribute, and background
-- attribute.
data Cell
  = Cell !Char !Attr !Attr
  deriving (Eq)

instance Show Cell where
  show (Cell ch fg bg) =
    "Cell " ++ show ch ++ " " ++ show (attrToWord fg) ++ " " ++ show (attrToWord bg)

instance Storable Cell where
  sizeOf :: Cell -> Int
  sizeOf _ =
    Termbox.C.sizeofCell

  alignment :: Cell -> Int
  alignment _ =
    Termbox.C.alignofCell

  peek :: Ptr Cell -> IO Cell
  peek ptr =
    Cell
      <$> Termbox.C.getCellCh ptr
      <*> (wordToAttr <$> Termbox.C.getCellFg ptr)
      <*> (wordToAttr <$> Termbox.C.getCellBg ptr)

  poke :: Ptr Cell -> Cell -> IO ()
  poke ptr (Cell ch fg bg) = do
    Termbox.C.setCellCh ptr ch
    Termbox.C.setCellFg ptr (attrToWord fg)
    Termbox.C.setCellBg ptr (attrToWord bg)

-- | Set the cell at the given coordinates (column, then row).
set :: Int -> Int -> Cell -> IO ()
set x y (Cell ch fg bg) =
  tb_change_cell x y ch (attrToWord fg) (attrToWord bg)

-- | Get the terminal's two-dimensional array of cells (indexed by row, then
-- column).
getCells :: IO (Array (Int, Int) Cell)
getCells =
  join (mkbuffer <$> (tb_cell_buffer >>= newForeignPtr_) <*> tb_width <*> tb_height)
  where
    mkbuffer :: ForeignPtr Cell -> Int -> Int -> IO (Array (Int, Int) Cell)
    mkbuffer buff w h =
      Array.freeze =<< Array.unsafeForeignPtrToStorableArray buff ((0, 0), (h -1, w -1))

foreign import ccall safe "termbox.h tb_cell_buffer"
  tb_cell_buffer :: IO (Ptr Cell)

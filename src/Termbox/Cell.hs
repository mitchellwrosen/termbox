{-# LANGUAGE TypeApplications #-}

module Termbox.Cell
  ( Cell (..),
    getCells,
  )
where

import Control.Monad (join)
import Data.Array (Array)
import qualified Data.Array.Storable as Array (freeze)
import qualified Data.Array.Storable.Internals as Array
import Data.Char (chr, ord)
import Data.Word (Word32)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import Foreign.Ptr (Ptr)
import Foreign.Storable
import Termbox.Attr (Attr, attrToWord, wordToAttr)
import Termbox.Internal

-- | A cell contains a character, foreground attribute, and background attribute.
data Cell
  = Cell !Char !Attr !Attr
  deriving (Eq, Show)

instance Storable Cell where
  sizeOf :: Cell -> Int
  sizeOf _ =
    8

  alignment :: Cell -> Int
  alignment _ =
    4

  peek :: Ptr Cell -> IO Cell
  peek ptr = do
    Cell
      <$> (chr . fromIntegral @Word32 @Int <$> peekByteOff ptr 0)
      <*> (wordToAttr <$> peekByteOff ptr 4)
      <*> (wordToAttr <$> peekByteOff ptr 6)

  poke :: Ptr Cell -> Cell -> IO ()
  poke ptr (Cell ch fg bg) = do
    pokeByteOff ptr 0 (fromIntegral @Int @Word32 (ord ch))
    pokeByteOff ptr 4 (attrToWord fg)
    pokeByteOff ptr 6 (attrToWord bg)

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

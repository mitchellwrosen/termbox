{-# LANGUAGE TypeApplications #-}

module Termbox.Cell
  ( Cell (..),
  )
where

import Data.Char (chr, ord)
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.Storable
import Termbox.Attr (Attr, attrToWord, wordToAttr)

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

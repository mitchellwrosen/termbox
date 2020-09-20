module Termbox.Cursor
  ( Cursor (..),
  )
where

import Termbox.Internal

-- | A cursor.
data Cursor
  = -- | Column, then row
    Cursor !Int !Int
  | NoCursor

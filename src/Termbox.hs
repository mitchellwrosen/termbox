{-# language DeriveAnyClass      #-}
{-# language DerivingStrategies  #-}
{-# language InstanceSigs        #-}
{-# language LambdaCase          #-}
{-# language RankNTypes          #-}
{-# language RecordWildCards     #-}
{-# language ScopedTypeVariables #-}
{-# language UnicodeSyntax       #-}

module Termbox
  ( -- * Initialization
    main
    -- * Terminal size
  , size
    -- * Cursor
  , setCursor
  , hideCursor
    -- * Terminal contents
  , Cell(..)
  , setCell
  , cellBuffer
  , clear
  , flush
    -- * Terminal mode
  , OutputMode(..)
  , getOutputMode
  , selectOutputMode
    -- * Event handling
  , Event(..)
  , Key(..)
  , pollEvent
    -- * Attributes
  , Attr
  , black
  , red
  , green
  , yellow
  , blue
  , magenta
  , cyan
  , white
  , bold
  , underline
  , reverse
    -- * Exceptions
  , InitError(..)
  ) where

import Prelude hiding (mod, reverse)

import qualified Termbox.Internal as Tb

import Control.Exception
import Control.Monad (join)
import Data.Array.Storable
import Data.Bits ((.|.), (.&.))
import Data.Functor (void)
import Data.Word
import Foreign (ForeignPtr, Ptr, newForeignPtr_)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable

import qualified Data.Array.Storable.Internals as Array

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Initialization errors that can be thrown by 'main'.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Run a @termbox@ program. May throw an 'InitError' exception.
main :: IO a -> IO a
main =
  bracket_
    (Tb.init >>= \case
      Tb.InitOk -> pure ()
      Tb.FailedToOpenTTY -> throwIO FailedToOpenTTY
      Tb.PipeTrapError -> throwIO PipeTrapError
      Tb.UnsupportedTerminal -> throwIO UnsupportedTerminal)
    Tb.shutdown

--------------------------------------------------------------------------------
-- Terminal size
--------------------------------------------------------------------------------

-- | Get the terminal width and height.
size :: IO (Int, Int)
size =
  (,) <$> Tb.width <*> Tb.height

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

-- | Set the cursor coordinates.
setCursor :: Int -> Int -> IO ()
setCursor =
  Tb.setCursor

-- | Hide the cursor.
hideCursor :: IO ()
hideCursor =
  Tb.setCursor Tb._HIDE_CURSOR Tb._HIDE_CURSOR

--------------------------------------------------------------------------------
-- Terminal contents
--------------------------------------------------------------------------------

-- | A 'Cell' contains a character, foreground attribute, and background
-- attribute.
data Cell
  = Cell !Char !Attr !Attr

instance Show Cell where
  show (Cell ch fg bg) =
    "Cell " ++ show ch ++ " " ++ show (attrToWord fg) ++ " " ++
      show (attrToWord bg)

instance Storable Cell where
  sizeOf :: Cell -> Int
  sizeOf _ =
    Tb.sizeofCell

  alignment :: Cell -> Int
  alignment _ =
    Tb.alignofCell

  peek :: Ptr Cell -> IO Cell
  peek ptr =
    Cell
      <$> Tb.getCellCh ptr
      <*> (wordToAttr <$>  Tb.getCellFg ptr)
      <*> (wordToAttr <$> Tb.getCellBg ptr)

  poke :: Ptr Cell -> Cell -> IO ()
  poke ptr (Cell ch fg bg) = do
    Tb.setCellCh ptr ch
    Tb.setCellFg ptr (attrToWord fg)
    Tb.setCellFg ptr (attrToWord bg)

-- | Set the 'Cell' at the given coordinates.
setCell
  :: Int -- ^ Column
  -> Int -- ^ Row
  -> Cell -- ^ Cell
  -> IO ()
setCell x y (Cell ch fg bg) =
  Tb.changeCell x y ch (attrToWord fg) (attrToWord bg)

-- | Get the terminal's internal back buffer as a two-dimensional array of
-- 'Cell's indexed by their @x@ and @y@ coordinates.
--
--
-- *Warning* The data is only valid until the next call to 'clear' or 'flush'.
cellBuffer :: IO (StorableArray (Int, Int) Cell)
cellBuffer =
  join
    (mkbuffer
      <$> (tb_cell_buffer >>= newForeignPtr_)
      <*> Tb.width
      <*> Tb.height)
 where
  mkbuffer
    :: ForeignPtr Cell
    -> Int
    -> Int
    -> IO (StorableArray (Int, Int) Cell)
  mkbuffer buffer w h =
    Array.unsafeForeignPtrToStorableArray buffer ((0, 0), (w-1, h-1))

-- | Clear the back buffer with the given foreground and background attributes.
clear
  :: Attr -- ^ Foreground
  -> Attr -- ^ Background
  -> IO ()
clear fg bg = do
  Tb.setClearAttributes (attrToWord fg) (attrToWord bg)
  Tb.clear

-- | Synchronize the internal back buffer with the terminal.
flush :: IO ()
flush =
  Tb.present

--------------------------------------------------------------------------------
-- Terminal mode
--------------------------------------------------------------------------------

data OutputMode
  = OutputModeNormal
  | OutputMode256
  | OutputMode216
  | OutputModeGrayscale
  deriving (Eq, Ord, Show)

getOutputMode :: IO OutputMode
getOutputMode =
  f <$> Tb.selectOutputMode Tb.OutputModeCurrent
 where
  f :: Tb.OutputMode -> OutputMode
  f = \case
    Tb.OutputModeNormal -> OutputModeNormal
    Tb.OutputMode256 -> OutputMode256
    Tb.OutputMode216 -> OutputMode216
    Tb.OutputModeGrayscale -> OutputModeGrayscale
    Tb.OutputModeCurrent -> error "getOutputMode: OutputModeCurrent"

selectOutputMode :: OutputMode -> IO ()
selectOutputMode mode =
  void (Tb.selectOutputMode (f mode))
 where
  f :: OutputMode -> Tb.OutputMode
  f = \case
    OutputModeNormal -> Tb.OutputModeNormal
    OutputMode256 -> Tb.OutputMode256
    OutputMode216 -> Tb.OutputMode216
    OutputModeGrayscale -> Tb.OutputModeGrayscale

--------------------------------------------------------------------------------
-- Event handling
--------------------------------------------------------------------------------

data Event
  = EventKey Key !Bool
  | EventResize -- TODO
  deriving (Eq, Show)

data Key
  = KeyChar Char
  | KeyArrowDown
  | KeyArrowLeft
  | KeyArrowRight
  | KeyArrowUp
  | KeyBackspace
  | KeyBackspace2
  | KeyCtrl2
  | KeyCtrl3
  | KeyCtrl4
  | KeyCtrl5
  | KeyCtrl6
  | KeyCtrl7
  | KeyCtrl8
  | KeyCtrlA
  | KeyCtrlB
  | KeyCtrlBackslash
  | KeyCtrlC
  | KeyCtrlD
  | KeyCtrlE
  | KeyCtrlF
  | KeyCtrlG
  | KeyCtrlH
  | KeyCtrlI
  | KeyCtrlJ
  | KeyCtrlK
  | KeyCtrlL
  | KeyCtrlLsqBracket
  | KeyCtrlM
  | KeyCtrlN
  | KeyCtrlO
  | KeyCtrlP
  | KeyCtrlQ
  | KeyCtrlR
  | KeyCtrlRsqBracket
  | KeyCtrlS
  | KeyCtrlSlash
  | KeyCtrlT
  | KeyCtrlTilde
  | KeyCtrlU
  | KeyCtrlUnderscore
  | KeyCtrlV
  | KeyCtrlW
  | KeyCtrlX
  | KeyCtrlY
  | KeyCtrlZ
  | KeyDelete
  | KeyEnd
  | KeyEnter
  | KeyEsc
  | KeyF1
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyHome
  | KeyInsert
  | KeyMouseLeft
  | KeyMouseMiddle
  | KeyMouseRelease
  | KeyMouseRight
  | KeyMouseWheelDown
  | KeyMouseWheelUp
  | KeyPageDn
  | KeyPageUp
  | KeySpace
  | KeyTab
  deriving (Eq, Ord, Read, Show)


pollEvent :: IO Event
pollEvent =
  alloca $ \ptr ->
    Tb.pollEvent ptr >>= \case
      -1 ->
        throwIO (userError "pollEvent -1")
      _ ->
        parseEvent <$> peek ptr

-- | Parse an 'Event' from a 'Tb.Event'.
parseEvent :: Tb.Event -> Event
parseEvent = \case
  Tb.Event Tb.EventKey mod key ch _ _ _ _ ->
    parseEventKey mod key ch
  Tb.Event Tb.EventResize _ _ _ _ _ _ _ ->
    undefined
  Tb.Event Tb.EventMouse _ _ _ _ _ _ _ ->
    undefined

-- | Parse a key 'Event'.
parseEventKey :: Tb.Mod -> Tb.Key -> Char -> Event
parseEventKey mod key ch =
  EventKey key' alt
 where
  key' :: Key
  key' =
    case ch of
      '\0' -> parseKey key
      _ -> KeyChar ch

  alt :: Bool
  alt =
    case mod of
      Tb.ModAlt -> True
      _ -> False

-- | Parse a 'Key' from a 'Tb.Key'.
parseKey :: Tb.Key -> Key
parseKey = \case
  Tb.KeyArrowDown -> KeyArrowDown
  Tb.KeyArrowLeft -> KeyArrowLeft
  Tb.KeyArrowRight -> KeyArrowRight
  Tb.KeyArrowUp -> KeyArrowUp
  Tb.KeyBackspace -> KeyBackspace
  Tb.KeyBackspace2 -> KeyBackspace2
  Tb.KeyCtrl2 -> KeyCtrl2
  Tb.KeyCtrl3 -> KeyCtrl3
  Tb.KeyCtrl4 -> KeyCtrl4
  Tb.KeyCtrl5 -> KeyCtrl5
  Tb.KeyCtrl6 -> KeyCtrl6
  Tb.KeyCtrl7 -> KeyCtrl7
  Tb.KeyCtrl8 -> KeyCtrl8
  Tb.KeyCtrlA -> KeyCtrlA
  Tb.KeyCtrlB -> KeyCtrlB
  Tb.KeyCtrlBackslash -> KeyCtrlBackslash
  Tb.KeyCtrlC -> KeyCtrlC
  Tb.KeyCtrlD -> KeyCtrlD
  Tb.KeyCtrlE -> KeyCtrlE
  Tb.KeyCtrlF -> KeyCtrlF
  Tb.KeyCtrlG -> KeyCtrlG
  Tb.KeyCtrlH -> KeyCtrlH
  Tb.KeyCtrlI -> KeyCtrlI
  Tb.KeyCtrlJ -> KeyCtrlJ
  Tb.KeyCtrlK -> KeyCtrlK
  Tb.KeyCtrlL -> KeyCtrlL
  Tb.KeyCtrlLsqBracket -> KeyCtrlLsqBracket
  Tb.KeyCtrlM -> KeyCtrlM
  Tb.KeyCtrlN -> KeyCtrlN
  Tb.KeyCtrlO -> KeyCtrlO
  Tb.KeyCtrlP -> KeyCtrlP
  Tb.KeyCtrlQ -> KeyCtrlQ
  Tb.KeyCtrlR -> KeyCtrlR
  Tb.KeyCtrlRsqBracket -> KeyCtrlRsqBracket
  Tb.KeyCtrlS -> KeyCtrlS
  Tb.KeyCtrlSlash -> KeyCtrlSlash
  Tb.KeyCtrlT -> KeyCtrlT
  Tb.KeyCtrlTilde -> KeyCtrlTilde
  Tb.KeyCtrlU -> KeyCtrlU
  Tb.KeyCtrlUnderscore -> KeyCtrlUnderscore
  Tb.KeyCtrlV -> KeyCtrlV
  Tb.KeyCtrlW -> KeyCtrlW
  Tb.KeyCtrlX -> KeyCtrlX
  Tb.KeyCtrlY -> KeyCtrlY
  Tb.KeyCtrlZ -> KeyCtrlZ
  Tb.KeyDelete -> KeyDelete
  Tb.KeyEnd -> KeyEnd
  Tb.KeyEnter -> KeyEnter
  Tb.KeyEsc -> KeyEsc
  Tb.KeyF1 -> KeyF1
  Tb.KeyF10 -> KeyF10
  Tb.KeyF11 -> KeyF11
  Tb.KeyF12 -> KeyF12
  Tb.KeyF2 -> KeyF2
  Tb.KeyF3 -> KeyF3
  Tb.KeyF4 -> KeyF4
  Tb.KeyF5 -> KeyF5
  Tb.KeyF6 -> KeyF6
  Tb.KeyF7 -> KeyF7
  Tb.KeyF8 -> KeyF8
  Tb.KeyF9 -> KeyF9
  Tb.KeyHome -> KeyHome
  Tb.KeyInsert -> KeyInsert
  Tb.KeyMouseLeft -> KeyMouseLeft
  Tb.KeyMouseMiddle -> KeyMouseMiddle
  Tb.KeyMouseRelease -> KeyMouseRelease
  Tb.KeyMouseRight -> KeyMouseRight
  Tb.KeyMouseWheelDown -> KeyMouseWheelDown
  Tb.KeyMouseWheelUp -> KeyMouseWheelUp
  Tb.KeyPageDn -> KeyPageDn
  Tb.KeyPageUp -> KeyPageUp
  Tb.KeySpace -> KeySpace
  Tb.KeyTab -> KeyTab

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

data Attr
  = Attr !Word16 {- color -} !Word16 {- attr -}
  deriving (Eq)

instance Monoid Attr where
  mempty :: Attr
  mempty =
    Attr Tb._DEFAULT 0

instance Num Attr where
  fromInteger :: Integer -> Attr
  fromInteger n
    | n >= 0 && n < 256 =
        Attr (fromIntegral n) 0
    | otherwise =
        error ("Attr.fromInteger: " ++ show n ++ " out of range [0..255]")

  (+) = error ("Attr.(+): not defined")
  (*) = error ("Attr.(*): not defined")
  (-) = error ("Attr.(-): not defined")
  abs = error ("Attr.abs: not defined")
  signum = error ("Attr.signum: not defined")

-- | Right-biased color; attributes are merged.
instance Semigroup Attr where
  (<>) :: Attr -> Attr -> Attr
  Attr  0 ax <> Attr cy ay = Attr cy (ax .|. ay)
  Attr cx ax <> Attr  0 ay = Attr cx (ax .|. ay)
  Attr  _ ax <> Attr cy ay = Attr cy (ax .|. ay)

wordToAttr :: Word16 -> Attr
wordToAttr w =
  Attr (w .&. 0x00FF) (w .&. 0xFF00)

attrToWord :: Attr -> Word16
attrToWord (Attr x y) =
  x .|. y

black :: Attr
black =
  Attr Tb._BLACK 0

red :: Attr
red =
  Attr Tb._RED 0

green :: Attr
green =
  Attr Tb._GREEN 0

yellow :: Attr
yellow =
  Attr Tb._YELLOW 0

blue :: Attr
blue =
  Attr Tb._BLUE 0

magenta :: Attr
magenta =
  Attr Tb._MAGENTA 0

cyan :: Attr
cyan =
  Attr Tb._CYAN 0

white :: Attr
white =
  Attr Tb._WHITE 0

bold :: Attr
bold =
  Attr Tb._DEFAULT Tb._BOLD

underline :: Attr
underline =
  Attr Tb._DEFAULT Tb._UNDERLINE

reverse :: Attr
reverse =
  Attr Tb._DEFAULT Tb._REVERSE

--------------------------------------------------------------------------------
-- Foreign imports
--------------------------------------------------------------------------------

foreign import ccall safe "termbox.h tb_cell_buffer"
  tb_cell_buffer :: IO (Ptr Cell)

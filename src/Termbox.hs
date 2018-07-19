{-# language InstanceSigs        #-}
{-# language LambdaCase          #-}
{-# language RankNTypes          #-}
{-# language RecordWildCards     #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies        #-}
{-# language UnicodeSyntax       #-}

module Termbox
  ( -- $intro

    -- * Initialization
    main
  , InitError(..)
    -- * Terminal contents
  , Cell(..)
  , set
  , buffer
  , clear
  , flush
    -- * Terminal size
  , size
    -- * Cursor manipulation
  , setCursor
  , hideCursor
    -- * Event handling
  , Event(..)
  , Key(..)
  , poll
  , PollError(..)
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
    -- * Terminal modes
  , InputMode(..)
  , MouseMode(..)
  , getInputMode
  , setInputMode
  , OutputMode(..)
  , getOutputMode
  , setOutputMode
  ) where

import Prelude hiding (mod, reverse)

import qualified Termbox.Internal as Tb

import Control.Exception
import Control.Monad (join)
import Data.Array.Storable
import Data.Bits ((.|.), (.&.))
import Data.Functor (void)
import Data.Semigroup (Semigroup(..))
import Data.Word
import Foreign (ForeignPtr, Ptr, newForeignPtr_)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable

import qualified Data.Array.Storable.Internals as Array

-- $intro
-- This module is intended to be imported qualified.
--
-- @
-- import qualified Termbox
-- @

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Initialization errors that can be thrown by 'main'.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving (Show)

instance Exception InitError

-- | Run a @termbox@ program and restore the terminal state afterwards. May
-- throw an 'InitError' exception.
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
size :: (width ~ Int, height ~ Int) => IO (width, height)
size =
  (,) <$> Tb.width <*> Tb.height

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

-- | Set the cursor coordinates.
setCursor :: (col ~ Int, row ~ Int) => col -> row -> IO ()
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
  deriving (Eq)

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
    Tb.setCellBg ptr (attrToWord bg)

-- | Set the 'Cell' at the given coordinates.
set :: (col ~ Int, row ~ Int) => col -> row -> Cell -> IO ()
set x y (Cell ch fg bg) =
  Tb.changeCell x y ch (attrToWord fg) (attrToWord bg)

-- | Get the terminal's internal back buffer as a two-dimensional array of
-- 'Cell's indexed by their coordinates.
--
-- /Warning/: the data is only valid until the next call to 'clear' or
-- 'flush'.
buffer :: (row ~ Int, col ~ Int) => IO (StorableArray (row, col) Cell)
buffer =
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
  mkbuffer buff w h =
    Array.unsafeForeignPtrToStorableArray buff ((0, 0), (h-1, w-1))

-- | Clear the back buffer with the given foreground and background attributes.
clear :: (fg ~ Attr, bg ~ Attr) => fg -> bg -> IO ()
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

-- | The input modes.
--
-- * __Esc__. When ESC sequence is in the buffer and it doesn't match any known
-- sequence, ESC means 'KeyEsc'. This is the default input mode.
--
-- * __Alt__. When ESC sequence is in the buffer and it doesn't match any known
-- sequence, ESC enables the /alt/ modifier for the next keyboard event.
data InputMode
  = InputModeEsc MouseMode
  | InputModeAlt MouseMode
  deriving (Eq, Ord, Show)

-- | The mouse mode.
--
-- * __Yes__. Handle mouse events.
--
-- * __No__. Don't handle mouse events.
data MouseMode
  = MouseModeYes
  | MouseModeNo
  deriving (Eq, Ord, Show)

-- | Get the current 'InputMode'.
getInputMode :: IO InputMode
getInputMode =
  f <$> Tb.selectInputMode Tb._INPUT_CURRENT
 where
  f :: Int -> InputMode
  f = \case
    1 -> InputModeEsc MouseModeNo
    2 -> InputModeAlt MouseModeNo
    5 -> InputModeEsc MouseModeYes
    6 -> InputModeAlt MouseModeYes
    n -> error ("getInputMode: " ++ show n)

-- | Set the 'InputMode'.
setInputMode :: InputMode -> IO ()
setInputMode =
  void . Tb.selectInputMode . f
 where
  f :: InputMode -> Int
  f = \case
    InputModeEsc MouseModeNo -> Tb._INPUT_ESC
    InputModeEsc MouseModeYes -> Tb._INPUT_ESC .|. Tb._INPUT_MOUSE
    InputModeAlt MouseModeNo -> Tb._INPUT_ALT
    InputModeAlt MouseModeYes -> Tb._INPUT_ALT .|. Tb._INPUT_MOUSE

-- | The output modes.
--
-- * __Normal__. Supports colors /0..8/, which includes all named color
-- 'Attr's exported by this library, e.g. 'red'.
--
-- * __Grayscale__. Supports colors /0..23/.
--
-- * __216__. Supports colors /0..216/.
--
-- * __256__. Supports colors /0..255/.
data OutputMode
  = OutputModeNormal
  | OutputModeGrayscale
  | OutputMode216
  | OutputMode256
  deriving (Eq, Ord, Show)

-- | Get the current 'OutputMode'.
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

-- | Set the 'OutputMode'.
setOutputMode :: OutputMode -> IO ()
setOutputMode =
  void . Tb.selectOutputMode . f
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

-- | A input event.
data Event
  = EventKey !Key !Bool -- ^ Key event
  | EventResize !Int !Int -- ^ Resize event (width, then height)
  | EventMouse !Mouse !Int !Int -- ^ Mouse event (column, then row)
  deriving (Eq, Show)

-- | A key press.
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
  | KeyPageDn
  | KeyPageUp
  | KeySpace
  | KeyTab
  deriving (Eq, Ord, Show)

-- | A mouse event.
data Mouse
  = MouseLeft
  | MouseMiddle
  | MouseRelease
  | MouseRight
  | MouseWheelDown
  | MouseWheelUp
  deriving (Eq, Ord, Show)

-- | Block until an 'Event' arrives.
--
-- /Note/ @termbox v1.1.2@ does not properly handle OS signals that interrupt
-- the underlying @select@ system call, so unfortunately the familiar @Ctrl-C@
-- will not be able to stop a program stuck in 'pollEvent'.
--
-- You can work around this issue by polling in a background thread using the
-- @threaded@ runtime, or simply writing event-handling code that is responsive
-- to intuitive "quit" keys like @q@ and @Esc@.
--
-- This function may throw a 'PollError' exception under mysterious
-- circumstances that are not well-documented in the original C codebase.
poll :: IO Event
poll =
  alloca $ \ptr ->
    Tb.pollEvent ptr >>= \case
      -1 ->
        throwIO PollError
      _ ->
        parseEvent <$> peek ptr

data PollError
  = PollError
  deriving Show

instance Exception PollError

-- | Parse an 'Event' from a 'Tb.Event'.
parseEvent :: Tb.Event -> Event
parseEvent = \case
  Tb.Event Tb.EventKey mod key ch _ _ _ _ ->
    parseEventKey mod key ch
  Tb.Event Tb.EventResize _ _ _ w h _ _ ->
    EventResize w h
  Tb.Event Tb.EventMouse _ key _ _ _ x y ->
    EventMouse (parseMouse key) x y

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
  Tb.KeyPageDn -> KeyPageDn
  Tb.KeyPageUp -> KeyPageUp
  Tb.KeySpace -> KeySpace
  Tb.KeyTab -> KeyTab
  key -> error ("parseKey: " ++ show key)

-- | Parse a 'Mouse' from a 'Tb.Key'.
parseMouse :: Tb.Key -> Mouse
parseMouse = \case
  Tb.KeyMouseLeft -> MouseLeft
  Tb.KeyMouseMiddle -> MouseMiddle
  Tb.KeyMouseRelease -> MouseRelease
  Tb.KeyMouseRight -> MouseRight
  Tb.KeyMouseWheelDown -> MouseWheelDown
  Tb.KeyMouseWheelUp -> MouseWheelUp
  key -> error ("parseMouse: " ++ show key)

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

-- | A cell attribute, which includes its color, and whether or not it is
-- bold, underlined, and/or reversed.
--
-- A cell can only have one color, but may be (for example) bold /and/
-- underlined. The 'Monoid' instance combines 'Attr's this way, with a left
-- bias. That is,
--
-- @
-- red <> bold <> black <> underline = red <> bold <> underline
-- @
--
-- __Warning__: the 'Num' instance is /very partial/! It only includes an
-- implementation of 'fromInteger', for numeric literals.
data Attr
  = Attr !Word16 {- color -} !Word16 {- attr -}
  deriving (Eq)

instance Monoid Attr where
  mempty :: Attr
  mempty =
    Attr Tb._DEFAULT 0

  mappend :: Attr -> Attr -> Attr
  mappend =
    (<>)

-- | Only 'fromInteger' defined.
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

-- | Left-biased color; attributes are merged.
instance Semigroup Attr where
  (<>) :: Attr -> Attr -> Attr
  Attr  0 ax <> Attr cy ay = Attr cy (ax .|. ay)
  Attr cx ax <> Attr  0 ay = Attr cx (ax .|. ay)
  Attr cx ax <> Attr  _ ay = Attr cx (ax .|. ay)

wordToAttr :: Word16 -> Attr
wordToAttr w =
  Attr (w .&. 0x00FF) (w .&. 0xFF00)

attrToWord :: Attr -> Word16
attrToWord (Attr x y) =
  x .|. y

-- | @black = 1@.
black :: Attr
black =
  Attr Tb._BLACK 0

-- | @red = 2@.
red :: Attr
red =
  Attr Tb._RED 0

-- | @green = 3@.
green :: Attr
green =
  Attr Tb._GREEN 0

-- | @yellow = 4@.
yellow :: Attr
yellow =
  Attr Tb._YELLOW 0

-- | @blue = 5@.
blue :: Attr
blue =
  Attr Tb._BLUE 0

-- | @magenta = 6@.
magenta :: Attr
magenta =
  Attr Tb._MAGENTA 0

-- | @cyan = 7@.
cyan :: Attr
cyan =
  Attr Tb._CYAN 0

-- | @white = 8@.
white :: Attr
white =
  Attr Tb._WHITE 0

-- | Bold.
bold :: Attr
bold =
  Attr Tb._DEFAULT Tb._BOLD

-- | Underline.
underline :: Attr
underline =
  Attr Tb._DEFAULT Tb._UNDERLINE

-- | Reverse.
reverse :: Attr
reverse =
  Attr Tb._DEFAULT Tb._REVERSE

--------------------------------------------------------------------------------
-- Foreign imports
--------------------------------------------------------------------------------

foreign import ccall safe "termbox.h tb_cell_buffer"
  tb_cell_buffer :: IO (Ptr Cell)

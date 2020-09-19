{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- A @termbox@ program is typically constructed as an infinite loop that:
--
-- 1. 'clear's the terminal backbuffer.
-- 2. Renders the program state by 'set'ting individual pixels.
-- 3. 'flush'es the backbuffer to the terminal.
-- 4. 'poll's for an event to update the program state.
--
-- For example, this progam simply displays the number of keys pressed, and
-- quits on @Esc@:
--
-- @
-- {-\# LANGUAGE LambdaCase \#-}
--
-- import Data.Foldable (for_)
-- import qualified Termbox
--
-- main :: IO ()
-- main =
--   Termbox.'run_' (loop 0)
--
-- loop :: Int -> IO ()
-- loop n = do
--   Termbox.'clear' mempty mempty
--   render n
--   Termbox.'flush'
--
--   Termbox.'poll' >>= \\case
--     Termbox.'EventKey' Termbox.'KeyEsc' _ ->
--       pure ()
--     _ ->
--       loop (n+1)
--
-- render :: Int -> IO ()
-- render n =
--   for_
--     (zip [0..] (show n))
--     (\\(i, c) ->
--       Termbox.'set' i 0 (Termbox.'Cell' c mempty mempty))
-- @
--
-- Other termbox features include cell attributes (style, color), cursor
-- display, and mouse click handling.
--
-- This module is intended to be imported qualified.
module Termbox
  ( -- * Initialization
    run,
    run_,
    InitError (..),

    -- * Terminal contents
    set,
    getCells,
    clear,
    flush,
    Cell (..),

    -- * Terminal size
    getSize,

    -- * Cursor manipulation
    setCursor,
    hideCursor,

    -- * Event handling
    poll,
    Event (..),
    Key (..),
    Mouse (..),
    PollError (..),

    -- * Attributes
    black,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    bold,
    underline,
    reverse,
    Attr,

    -- * Terminal modes
    getInputMode,
    setInputMode,
    InputMode (..),
    MouseMode (..),
    getOutputMode,
    setOutputMode,
    OutputMode (..),
  )
where

import Control.Exception
import Control.Monad ((>=>), join)
import Data.Array (Array)
import qualified Data.Array.Storable as Array (freeze)
import qualified Data.Array.Storable.Internals as Array
import Data.Bits ((.&.), (.|.))
import Data.Functor (void)
import Data.Semigroup (Semigroup (..))
import Data.Word
import Foreign (ForeignPtr, Ptr, newForeignPtr_)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable
import GHC.Stack
import qualified Termbox.C
import Prelude hiding (mod, reverse)

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- | Termbox initialization errors that can be returned by 'run'.
data InitError
  = FailedToOpenTTY
  | PipeTrapError
  | UnsupportedTerminal
  deriving (Show)

instance Exception InitError

-- | Run a @termbox@ program and restore the terminal state afterwards.
run :: IO a -> IO (Either InitError a)
run action =
  mask $ \unmask ->
    Termbox.C.init >>= \case
      Termbox.C.InitOk -> do
        result <- unmask action `onException` Termbox.C.shutdown
        Termbox.C.shutdown
        pure (Right result)
      Termbox.C.FailedToOpenTTY -> pure (Left FailedToOpenTTY)
      Termbox.C.PipeTrapError -> pure (Left PipeTrapError)
      Termbox.C.UnsupportedTerminal -> pure (Left UnsupportedTerminal)

-- | Like 'run', but throws 'InitError's as @IO@ exceptions.
run_ :: IO a -> IO a
run_ =
  run >=> either throwIO pure

--------------------------------------------------------------------------------
-- Terminal size
--------------------------------------------------------------------------------

-- | Get the terminal size (width, then height).
getSize :: IO (Int, Int)
getSize =
  (,) <$> Termbox.C.width <*> Termbox.C.height

--------------------------------------------------------------------------------
-- Cursor
--------------------------------------------------------------------------------

-- | Set the cursor coordinates (column, then row).
setCursor :: Int -> Int -> IO ()
setCursor =
  Termbox.C.setCursor

-- | Hide the cursor.
hideCursor :: IO ()
hideCursor =
  Termbox.C.setCursor Termbox.C._HIDE_CURSOR Termbox.C._HIDE_CURSOR

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
    "Cell " ++ show ch ++ " " ++ show (attrToWord fg) ++ " "
      ++ show (attrToWord bg)

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
  Termbox.C.changeCell x y ch (attrToWord fg) (attrToWord bg)

-- | Get the terminal's two-dimensional array of cells (indexed by row, then
-- column).
getCells :: IO (Array (Int, Int) Cell)
getCells =
  join
    ( mkbuffer
        <$> (tb_cell_buffer >>= newForeignPtr_)
        <*> Termbox.C.width
        <*> Termbox.C.height
    )
  where
    mkbuffer ::
      ForeignPtr Cell ->
      Int ->
      Int ->
      IO (Array (Int, Int) Cell)
    mkbuffer buff w h =
      Array.freeze
        =<< Array.unsafeForeignPtrToStorableArray buff ((0, 0), (h -1, w -1))

-- | Clear the back buffer with the given foreground and background attributes.
clear :: Attr -> Attr -> IO ()
clear fg bg = do
  Termbox.C.setClearAttributes (attrToWord fg) (attrToWord bg)
  Termbox.C.clear

-- | Synchronize the internal back buffer with the terminal.
flush :: IO ()
flush =
  Termbox.C.present

--------------------------------------------------------------------------------
-- Terminal mode
--------------------------------------------------------------------------------

-- | The input modes.
--
-- * __Esc__. When ESC sequence is in the buffer and it doesn't match any known
-- sequence, ESC means 'KeyEsc'.
--
-- * __Alt__. When ESC sequence is in the buffer and it doesn't match any known
-- sequence, ESC enables the /alt/ modifier for the next keyboard event.
data InputMode
  = -- | Default.
    InputModeEsc MouseMode
  | InputModeAlt MouseMode
  deriving (Eq, Ord, Show)

-- | The mouse mode.
--
-- * __No__. Don't handle mouse events.
--
-- * __Yes__. Handle mouse events.
data MouseMode
  = -- | Default.
    MouseModeNo
  | MouseModeYes
  deriving (Eq, Ord, Show)

-- | Get the current input mode.
getInputMode :: HasCallStack => IO InputMode
getInputMode =
  f <$> Termbox.C.selectInputMode Termbox.C._INPUT_CURRENT
  where
    f :: Int -> InputMode
    f = \case
      1 -> InputModeEsc MouseModeNo
      2 -> InputModeAlt MouseModeNo
      5 -> InputModeEsc MouseModeYes
      6 -> InputModeAlt MouseModeYes
      n -> error (show n)

-- | Set the input mode.
setInputMode :: InputMode -> IO ()
setInputMode =
  void . Termbox.C.selectInputMode . f
  where
    f :: InputMode -> Int
    f = \case
      InputModeEsc MouseModeNo -> Termbox.C._INPUT_ESC
      InputModeEsc MouseModeYes -> Termbox.C._INPUT_ESC .|. Termbox.C._INPUT_MOUSE
      InputModeAlt MouseModeNo -> Termbox.C._INPUT_ALT
      InputModeAlt MouseModeYes -> Termbox.C._INPUT_ALT .|. Termbox.C._INPUT_MOUSE

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
getOutputMode :: HasCallStack => IO OutputMode
getOutputMode =
  f <$> Termbox.C.selectOutputMode Termbox.C.OutputModeCurrent
  where
    f :: Termbox.C.OutputMode -> OutputMode
    f = \case
      Termbox.C.OutputModeNormal -> OutputModeNormal
      Termbox.C.OutputMode256 -> OutputMode256
      Termbox.C.OutputMode216 -> OutputMode216
      Termbox.C.OutputModeGrayscale -> OutputModeGrayscale
      Termbox.C.OutputModeCurrent -> error "OutputModeCurrent"

-- | Set the output mode.
setOutputMode :: OutputMode -> IO ()
setOutputMode =
  void . Termbox.C.selectOutputMode . f
  where
    f :: OutputMode -> Termbox.C.OutputMode
    f = \case
      OutputModeNormal -> Termbox.C.OutputModeNormal
      OutputMode256 -> Termbox.C.OutputMode256
      OutputMode216 -> Termbox.C.OutputMode216
      OutputModeGrayscale -> Termbox.C.OutputModeGrayscale

--------------------------------------------------------------------------------
-- Event handling
--------------------------------------------------------------------------------

-- | A input event.
data Event
  = -- | Key event. The bool indicates the /alt/ modifier.
    EventKey !Key !Bool
  | -- | Resize event (width, then height)
    EventResize !Int !Int
  | -- | Mouse event (column, then row)
    EventMouse !Mouse !Int !Int
  deriving (Eq, Show)

-- | A key event.
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
-- /Note/: @termbox v1.1.2@ does not properly handle OS signals that interrupt
-- the underlying @select@ system call, so unfortunately the familiar @Ctrl-C@
-- will not be able to stop a program stuck in 'pollEvent'.
--
-- You can work around this issue by polling in a background thread using the
-- @threaded@ runtime, or simply writing event-handling code that is responsive
-- to intuitive "quit" keys like @q@ and @Esc@.
--
-- /Throws/: 'PollError'
poll :: IO Event
poll =
  alloca $ \ptr ->
    Termbox.C.pollEvent ptr >>= \case
      -1 ->
        throwIO PollError
      _ ->
        parseEvent <$> peek ptr

-- | An error occurred when 'poll'ing, due to mysterious circumstances that are
-- not well-documented in the original C codebase.
data PollError
  = PollError
  deriving (Show)

instance Exception PollError

-- | Parse an 'Event' from a 'Tb.Event'.
parseEvent :: Termbox.C.Event -> Event
parseEvent = \case
  Termbox.C.Event Termbox.C.EventKey mod key ch _ _ _ _ ->
    parseEventKey mod key ch
  Termbox.C.Event Termbox.C.EventResize _ _ _ w h _ _ ->
    EventResize w h
  Termbox.C.Event Termbox.C.EventMouse _ key _ _ _ x y ->
    EventMouse (parseMouse key) x y

-- | Parse a key 'Event'.
parseEventKey :: Termbox.C.Mod -> Termbox.C.Key -> Char -> Event
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
        Termbox.C.ModAlt -> True
        _ -> False

-- | Parse a 'Key' from a 'Tb.Key'.
parseKey :: HasCallStack => Termbox.C.Key -> Key
parseKey = \case
  Termbox.C.KeyArrowDown -> KeyArrowDown
  Termbox.C.KeyArrowLeft -> KeyArrowLeft
  Termbox.C.KeyArrowRight -> KeyArrowRight
  Termbox.C.KeyArrowUp -> KeyArrowUp
  Termbox.C.KeyBackspace -> KeyBackspace
  Termbox.C.KeyBackspace2 -> KeyBackspace2
  Termbox.C.KeyCtrl2 -> KeyCtrl2
  Termbox.C.KeyCtrl3 -> KeyCtrl3
  Termbox.C.KeyCtrl4 -> KeyCtrl4
  Termbox.C.KeyCtrl5 -> KeyCtrl5
  Termbox.C.KeyCtrl6 -> KeyCtrl6
  Termbox.C.KeyCtrl7 -> KeyCtrl7
  Termbox.C.KeyCtrl8 -> KeyCtrl8
  Termbox.C.KeyCtrlA -> KeyCtrlA
  Termbox.C.KeyCtrlB -> KeyCtrlB
  Termbox.C.KeyCtrlBackslash -> KeyCtrlBackslash
  Termbox.C.KeyCtrlC -> KeyCtrlC
  Termbox.C.KeyCtrlD -> KeyCtrlD
  Termbox.C.KeyCtrlE -> KeyCtrlE
  Termbox.C.KeyCtrlF -> KeyCtrlF
  Termbox.C.KeyCtrlG -> KeyCtrlG
  Termbox.C.KeyCtrlH -> KeyCtrlH
  Termbox.C.KeyCtrlI -> KeyCtrlI
  Termbox.C.KeyCtrlJ -> KeyCtrlJ
  Termbox.C.KeyCtrlK -> KeyCtrlK
  Termbox.C.KeyCtrlL -> KeyCtrlL
  Termbox.C.KeyCtrlLsqBracket -> KeyCtrlLsqBracket
  Termbox.C.KeyCtrlM -> KeyCtrlM
  Termbox.C.KeyCtrlN -> KeyCtrlN
  Termbox.C.KeyCtrlO -> KeyCtrlO
  Termbox.C.KeyCtrlP -> KeyCtrlP
  Termbox.C.KeyCtrlQ -> KeyCtrlQ
  Termbox.C.KeyCtrlR -> KeyCtrlR
  Termbox.C.KeyCtrlRsqBracket -> KeyCtrlRsqBracket
  Termbox.C.KeyCtrlS -> KeyCtrlS
  Termbox.C.KeyCtrlSlash -> KeyCtrlSlash
  Termbox.C.KeyCtrlT -> KeyCtrlT
  Termbox.C.KeyCtrlTilde -> KeyCtrlTilde
  Termbox.C.KeyCtrlU -> KeyCtrlU
  Termbox.C.KeyCtrlUnderscore -> KeyCtrlUnderscore
  Termbox.C.KeyCtrlV -> KeyCtrlV
  Termbox.C.KeyCtrlW -> KeyCtrlW
  Termbox.C.KeyCtrlX -> KeyCtrlX
  Termbox.C.KeyCtrlY -> KeyCtrlY
  Termbox.C.KeyCtrlZ -> KeyCtrlZ
  Termbox.C.KeyDelete -> KeyDelete
  Termbox.C.KeyEnd -> KeyEnd
  Termbox.C.KeyEnter -> KeyEnter
  Termbox.C.KeyEsc -> KeyEsc
  Termbox.C.KeyF1 -> KeyF1
  Termbox.C.KeyF10 -> KeyF10
  Termbox.C.KeyF11 -> KeyF11
  Termbox.C.KeyF12 -> KeyF12
  Termbox.C.KeyF2 -> KeyF2
  Termbox.C.KeyF3 -> KeyF3
  Termbox.C.KeyF4 -> KeyF4
  Termbox.C.KeyF5 -> KeyF5
  Termbox.C.KeyF6 -> KeyF6
  Termbox.C.KeyF7 -> KeyF7
  Termbox.C.KeyF8 -> KeyF8
  Termbox.C.KeyF9 -> KeyF9
  Termbox.C.KeyHome -> KeyHome
  Termbox.C.KeyInsert -> KeyInsert
  Termbox.C.KeyPageDn -> KeyPageDn
  Termbox.C.KeyPageUp -> KeyPageUp
  Termbox.C.KeySpace -> KeySpace
  Termbox.C.KeyTab -> KeyTab
  key -> error (show key)

-- | Parse a 'Mouse' from a 'Tb.Key'.
parseMouse :: HasCallStack => Termbox.C.Key -> Mouse
parseMouse = \case
  Termbox.C.KeyMouseLeft -> MouseLeft
  Termbox.C.KeyMouseMiddle -> MouseMiddle
  Termbox.C.KeyMouseRelease -> MouseRelease
  Termbox.C.KeyMouseRight -> MouseRight
  Termbox.C.KeyMouseWheelDown -> MouseWheelDown
  Termbox.C.KeyMouseWheelUp -> MouseWheelUp
  key -> error (show key)

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

-- | A cell attribute, which includes its color, and whether or not it is
-- bold, underlined, and/or reversed.
--
-- A cell can only have one color, but may be (for example) bold /and/
-- underlined. The 'Monoid' instance combines 'Attr's this way, with a right
-- bias.
data Attr
  = Attr !Word16 {- color -} !Word16 {- attr -}
  deriving (Eq)

instance Monoid Attr where
  mempty :: Attr
  mempty =
    Attr Termbox.C._DEFAULT 0

  mappend :: Attr -> Attr -> Attr
  mappend =
    (<>)

-- | Provided for numeric literals.
instance Num Attr where
  fromInteger :: Integer -> Attr
  fromInteger n =
    Attr (fromIntegral (n `rem` 256)) 0

  (+) = (<>)
  (*) = (<>)
  (-) = (<>)
  abs = id
  signum = id

-- | Left-biased color; attributes are merged.
instance Semigroup Attr where
  (<>) :: Attr -> Attr -> Attr
  Attr 0 ax <> Attr cy ay = Attr cy (ax .|. ay)
  Attr cx ax <> Attr 0 ay = Attr cx (ax .|. ay)
  Attr _ ax <> Attr cy ay = Attr cy (ax .|. ay)

wordToAttr :: Word16 -> Attr
wordToAttr w =
  Attr (w .&. 0x00FF) (w .&. 0xFF00)

attrToWord :: Attr -> Word16
attrToWord (Attr x y) =
  x .|. y

-- | @black = 1@.
black :: Attr
black =
  Attr Termbox.C._BLACK 0

-- | @red = 2@.
red :: Attr
red =
  Attr Termbox.C._RED 0

-- | @green = 3@.
green :: Attr
green =
  Attr Termbox.C._GREEN 0

-- | @yellow = 4@.
yellow :: Attr
yellow =
  Attr Termbox.C._YELLOW 0

-- | @blue = 5@.
blue :: Attr
blue =
  Attr Termbox.C._BLUE 0

-- | @magenta = 6@.
magenta :: Attr
magenta =
  Attr Termbox.C._MAGENTA 0

-- | @cyan = 7@.
cyan :: Attr
cyan =
  Attr Termbox.C._CYAN 0

-- | @white = 8@.
white :: Attr
white =
  Attr Termbox.C._WHITE 0

-- | Bold modifier attribute.
bold :: Attr
bold =
  Attr Termbox.C._DEFAULT Termbox.C._BOLD

-- | Underline modifier attribute.
underline :: Attr
underline =
  Attr Termbox.C._DEFAULT Termbox.C._UNDERLINE

-- | Reverse modifier attribute.
reverse :: Attr
reverse =
  Attr Termbox.C._DEFAULT Termbox.C._REVERSE

--------------------------------------------------------------------------------
-- Foreign imports
--------------------------------------------------------------------------------

foreign import ccall safe "termbox.h tb_cell_buffer"
  tb_cell_buffer :: IO (Ptr Cell)

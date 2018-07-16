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
  , width
  , height
    -- * Cursor
  , setCursor
  , hideCursor
    -- * Terminal contents
  , putCell
  , clear
  , present
    -- * Terminal mode
  , OutputMode(..)
  , getOutputMode
  , selectOutputMode
    -- * Event handling
  , Event(..)
  , Key(..)
  , pollEvent
    -- * Colors
  , Color
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
import Control.Monad.Primitive
import Data.Bits ((.|.))
import Data.Functor (void)
import Data.Vector (MVector)
import Data.Word
import Foreign (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)

import qualified Data.Vector.Mutable as MVector

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

-- | Get the terminal width.
width :: IO Int
width =
  Tb.width

-- | Get the terminal height.
height :: IO Int
height =
  Tb.height

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

data Cell = Cell
  { cellCh :: !Char
  , cellFg :: !Color
  , cellBg :: !Color
  }
-- | Put a cell at the given coordinates.
putCell
  :: Int -- ^ Column
  -> Int -- ^ Row
  -> Char -- ^ Character
  -> Color -- ^ Foreground color
  -> Color -- ^ Background color
  -> IO ()
putCell x y ch fg bg =
  Tb.changeCell x y ch (colorToWord fg) (colorToWord bg)

withBuffer
  :: ∀ a. (∀ m. PrimMonad m => MVector (PrimState m) Cell -> m a)
  -> IO ()
withBuffer m = do
  buffer <- Tb.cellBuffer
  cells <- do
    w <- Tb.width
    h <- Tb.height
    MVector.new (w*h)
  pure ()

-- | Clear the back buffer with the given foreground and background attributes.
clear :: Color -> Color -> IO ()
clear fg bg = do
  Tb.setClearAttributes (colorToWord fg) (colorToWord bg)
  Tb.clear

-- | Display the back buffer.
present :: IO ()
present =
  Tb.present-- ^ Background color

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
-- Colors
--------------------------------------------------------------------------------

data Color
  = Color !Word16 {- color -} !Word16 {- attr -}
  deriving (Eq)

instance Monoid Color where
  mempty :: Color
  mempty =
    Color Tb._DEFAULT 0

instance Num Color where
  fromInteger :: Integer -> Color
  fromInteger n
    | n >= 0 && n < 256 =
        Color (fromIntegral n) 0
    | otherwise =
        error ("Color.fromInteger: " ++ show n ++ " out of range [0..255]")

  (+) = error ("Color.(+): not defined")
  (*) = error ("Color.(*): not defined")
  (-) = error ("Color.(-): not defined")
  abs = error ("Color.abs: not defined")
  signum = error ("Color.signum: not defined")

-- | Right-biased color; attributes are merged.
instance Semigroup Color where
  (<>) :: Color -> Color -> Color
  Color  0 ax <> Color cy ay = Color cy (ax .|. ay)
  Color cx ax <> Color  0 ay = Color cx (ax .|. ay)
  Color  _ ax <> Color cy ay = Color cy (ax .|. ay)

colorToWord :: Color -> Word16
colorToWord (Color x y) =
  x .|. y

black :: Color
black =
  Color Tb._BLACK 0

red :: Color
red =
  Color Tb._RED 0

green :: Color
green =
  Color Tb._GREEN 0

yellow :: Color
yellow =
  Color Tb._YELLOW 0

blue :: Color
blue =
  Color Tb._BLUE 0

magenta :: Color
magenta =
  Color Tb._MAGENTA 0

cyan :: Color
cyan =
  Color Tb._CYAN 0

white :: Color
white =
  Color Tb._WHITE 0

bold :: Color
bold =
  Color Tb._DEFAULT Tb._BOLD

underline :: Color
underline =
  Color Tb._DEFAULT Tb._UNDERLINE

reverse :: Color
reverse =
  Color Tb._DEFAULT Tb._REVERSE

-- | Lowest-level termbox bindings. No creativity here, just a 1:1 mapping.
-- Some of the enums are hand-written (source copied from generated module) so
-- I can insert haddocks.

{-# language InstanceSigs #-}

module Termbox.Internal where

#include <termbox.h>

import Data.Char (ord)
import Data.Word
import Foreign
import Foreign.C
import Prelude hiding (mod)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

_INPUT_CURRENT, _INPUT_ESC, _INPUT_ALT, _INPUT_MOUSE :: Int
_HIDE_CURSOR :: Int
_BOLD, _UNDERLINE, _REVERSE :: Word16
_DEFAULT, _BLACK, _RED, _GREEN, _YELLOW, _BLUE, _MAGENTA, _CYAN, _WHITE :: Word16

_INPUT_CURRENT = {# const TB_INPUT_CURRENT #}
_INPUT_ESC     = {# const TB_INPUT_ESC     #}
_INPUT_ALT     = {# const TB_INPUT_ALT     #}
_INPUT_MOUSE   = {# const TB_INPUT_MOUSE   #}

_HIDE_CURSOR   = {# const TB_HIDE_CURSOR #}

_BOLD          = {# const TB_BOLD #}
_UNDERLINE     = {# const TB_UNDERLINE #}
_REVERSE       = {# const TB_REVERSE #}

_DEFAULT       = {# const TB_DEFAULT #}
_BLACK         = {# const TB_BLACK #}
_RED           = {# const TB_RED #}
_GREEN         = {# const TB_GREEN #}
_YELLOW        = {# const TB_YELLOW #}
_BLUE          = {# const TB_BLUE #}
_MAGENTA       = {# const TB_MAGENTA #}
_CYAN          = {# const TB_CYAN #}
_WHITE         = {# const TB_WHITE #}

--------------------------------------------------------------------------------
-- Enums
--------------------------------------------------------------------------------

{#
  enum define EventType
    { TB_EVENT_KEY as EventKey
    , TB_EVENT_RESIZE as EventResize
    , TB_EVENT_MOUSE as EventMouse
    }
#}

{#
  enum define InitResult
    { 0 as InitOk
    , TB_EUNSUPPORTED_TERMINAL as UnsupportedTerminal
    , TB_EFAILED_TO_OPEN_TTY as FailedToOpenTTY
    , TB_EPIPE_TRAP_ERROR as PipeTrapError
    }
#}

{#
  enum define Key
    { TB_KEY_F1 as KeyF1
    , TB_KEY_F2 as KeyF2
    , TB_KEY_F3 as KeyF3
    , TB_KEY_F4 as KeyF4
    , TB_KEY_F5 as KeyF5
    , TB_KEY_F6 as KeyF6
    , TB_KEY_F7 as KeyF7
    , TB_KEY_F8 as KeyF8
    , TB_KEY_F9 as KeyF9
    , TB_KEY_F10 as KeyF10
    , TB_KEY_F11 as KeyF11
    , TB_KEY_F12 as KeyF12
    , TB_KEY_INSERT as KeyInsert
    , TB_KEY_DELETE as KeyDelete
    , TB_KEY_HOME as KeyHome
    , TB_KEY_END as KeyEnd
    , TB_KEY_PGUP as KeyPageUp
    , TB_KEY_PGDN as KeyPageDn
    , TB_KEY_ARROW_UP as KeyArrowUp
    , TB_KEY_ARROW_DOWN as KeyArrowDown
    , TB_KEY_ARROW_LEFT as KeyArrowLeft
    , TB_KEY_ARROW_RIGHT as KeyArrowRight
    , TB_KEY_MOUSE_LEFT as KeyMouseLeft
    , TB_KEY_MOUSE_RIGHT as KeyMouseRight
    , TB_KEY_MOUSE_MIDDLE as KeyMouseMiddle
    , TB_KEY_MOUSE_RELEASE as KeyMouseRelease
    , TB_KEY_MOUSE_WHEEL_UP as KeyMouseWheelUp
    , TB_KEY_MOUSE_WHEEL_DOWN as KeyMouseWheelDown
    , TB_KEY_CTRL_TILDE as KeyCtrlTilde
    , TB_KEY_CTRL_2 as KeyCtrl2
    , TB_KEY_CTRL_A as KeyCtrlA
    , TB_KEY_CTRL_B as KeyCtrlB
    , TB_KEY_CTRL_C as KeyCtrlC
    , TB_KEY_CTRL_D as KeyCtrlD
    , TB_KEY_CTRL_E as KeyCtrlE
    , TB_KEY_CTRL_F as KeyCtrlF
    , TB_KEY_CTRL_G as KeyCtrlG
    , TB_KEY_BACKSPACE as KeyBackspace
    , TB_KEY_CTRL_H as KeyCtrlH
    , TB_KEY_TAB as KeyTab
    , TB_KEY_CTRL_I as KeyCtrlI
    , TB_KEY_CTRL_J as KeyCtrlJ
    , TB_KEY_CTRL_K as KeyCtrlK
    , TB_KEY_CTRL_L as KeyCtrlL
    , TB_KEY_ENTER as KeyEnter
    , TB_KEY_CTRL_M as KeyCtrlM
    , TB_KEY_CTRL_N as KeyCtrlN
    , TB_KEY_CTRL_O as KeyCtrlO
    , TB_KEY_CTRL_P as KeyCtrlP
    , TB_KEY_CTRL_Q as KeyCtrlQ
    , TB_KEY_CTRL_R as KeyCtrlR
    , TB_KEY_CTRL_S as KeyCtrlS
    , TB_KEY_CTRL_T as KeyCtrlT
    , TB_KEY_CTRL_U as KeyCtrlU
    , TB_KEY_CTRL_V as KeyCtrlV
    , TB_KEY_CTRL_W as KeyCtrlW
    , TB_KEY_CTRL_X as KeyCtrlX
    , TB_KEY_CTRL_Y as KeyCtrlY
    , TB_KEY_CTRL_Z as KeyCtrlZ
    , TB_KEY_ESC as KeyEsc
    , TB_KEY_CTRL_LSQ_BRACKET as KeyCtrlLsqBracket
    , TB_KEY_CTRL_3 as KeyCtrl3
    , TB_KEY_CTRL_4 as KeyCtrl4
    , TB_KEY_CTRL_BACKSLASH as KeyCtrlBackslash
    , TB_KEY_CTRL_5 as KeyCtrl5
    , TB_KEY_CTRL_RSQ_BRACKET as KeyCtrlRsqBracket
    , TB_KEY_CTRL_6 as KeyCtrl6
    , TB_KEY_CTRL_7 as KeyCtrl7
    , TB_KEY_CTRL_SLASH as KeyCtrlSlash
    , TB_KEY_CTRL_UNDERSCORE as KeyCtrlUnderscore
    , TB_KEY_SPACE as KeySpace
    , TB_KEY_BACKSPACE2 as KeyBackspace2
    , TB_KEY_CTRL_8 as KeyCtrl8
    } deriving (Show)
#}

{#
  enum define Mod
    { 0 as ModNone
    , TB_MOD_ALT as ModAlt
    }
#}

{#
  enum define OutputMode
    { TB_OUTPUT_CURRENT as OutputModeCurrent
    , TB_OUTPUT_NORMAL as OutputModeNormal
    , TB_OUTPUT_256 as OutputMode256
    , TB_OUTPUT_216 as OutputMode216
    , TB_OUTPUT_GRAYSCALE as OutputModeGrayscale
    }

#}

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

sizeofCell :: Int
sizeofCell =
  {# sizeof tb_cell #}

alignofCell :: Int
alignofCell =
  {# alignof tb_cell #}

getCellCh :: Ptr a -> IO Char
getCellCh =
  fmap (toEnum . fromIntegral) . {# get tb_cell->ch #}

getCellFg :: Ptr a -> IO Word16
getCellFg =
  fmap fromIntegral . {# get tb_cell->fg #}

getCellBg :: Ptr a -> IO Word16
getCellBg =
  fmap fromIntegral . {# get tb_cell->bg #}

setCellCh :: Ptr a -> Char -> IO ()
setCellCh p =
  {# set tb_cell.ch #} p . fromIntegral . fromEnum

setCellFg :: Ptr a -> Word16 -> IO ()
setCellFg p =
  {# set tb_cell.fg #} p . fromIntegral

setCellBg :: Ptr a -> Word16 -> IO ()
setCellBg p =
  {# set tb_cell.bg #} p . fromIntegral

data Event
  = Event !EventType Mod Key Char Int Int Int Int

instance Storable Event where
  sizeOf :: Event -> Int
  sizeOf _ =
    {# sizeof tb_event #}

  alignment :: Event -> Int
  alignment _ =
    {# alignof tb_event #}

  peek :: Ptr Event -> IO Event
  peek p =
    Event
      <$> ((toEnum . fromIntegral) <$> {# get tb_event->type #} p)
      <*> ((toEnum . fromIntegral) <$> {# get tb_event->mod #} p)
      <*> ((toEnum . fromIntegral) <$> {# get tb_event->key #} p)
      <*> ((toEnum . fromIntegral) <$> {# get tb_event->ch #} p)
      <*> (fromIntegral <$> {# get tb_event->w #} p)
      <*> (fromIntegral <$> {# get tb_event->h #} p)
      <*> (fromIntegral <$> {# get tb_event->x #} p)
      <*> (fromIntegral <$> {# get tb_event->y #} p)

  poke :: Ptr Event -> Event -> IO ()
  poke p (Event typ mod key ch w h x y) = do
    {# set tb_event.type #} p (fromIntegral (fromEnum typ))
    {# set tb_event.mod #} p (fromIntegral (fromEnum mod))
    {# set tb_event.key #} p (fromIntegral (fromEnum key))
    {# set tb_event.ch #} p (fromIntegral (fromEnum ch))
    {# set tb_event.w #} p (fromIntegral w)
    {# set tb_event.h #} p (fromIntegral h)
    {# set tb_event.x #} p (fromIntegral x)
    {# set tb_event.y #} p (fromIntegral y)

{# pointer *tb_event as EventPtr -> Event #}

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

{#
  fun tb_change_cell as changeCell
    { `Int', `Int', charToUInt `Char', `Word16', `Word16' } -> `()'
#}

{#
  fun tb_clear as clear
    { } -> `()'
#}

{#
  fun tb_height as height
    { } -> `Int'
#}

{#
  fun tb_init as init
    { } -> `InitResult'
#}

{#
  fun tb_peek_event as peekEvent
    { `EventPtr', `Int' } -> `Int'
#}

{#
  fun tb_poll_event as pollEvent
    { `EventPtr' } -> `Int'
#}

{#
  fun tb_present as present
    { } -> `()'
#}

{#
  fun tb_select_input_mode as selectInputMode
    { `Int' } -> `Int'
#}

{#
  fun tb_select_output_mode as selectOutputMode
    { `OutputMode' } -> `OutputMode'
#}

{#
  fun tb_set_clear_attributes as setClearAttributes
    { `Word16', `Word16' } -> `()'
#}

{#
  fun tb_set_cursor as setCursor
    { `Int', `Int' } -> `()'
#}

{#
  fun tb_shutdown as shutdown
    { } -> `()'
#}

{#
  fun tb_width as width
    { } -> `Int'
#}

--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

charToUInt :: Char -> CUInt
charToUInt =
  fromIntegral . ord

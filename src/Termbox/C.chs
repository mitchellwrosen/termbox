-- | Lowest-level termbox bindings. No creativity here, just a 1:1 mapping.
-- Some of the enums are hand-written (source copied from generated module) so
-- I can insert haddocks.

{-# language InstanceSigs #-}

module Termbox.C where

#include <termbox.h>

import Data.Char (ord)
import Data.Word
import Foreign
import Foreign.C
import Prelude hiding (mod)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

tB_INPUT_CURRENT, tB_INPUT_ESC, tB_INPUT_ALT, tB_INPUT_MOUSE :: Int
tB_HIDE_CURSOR :: Int
tB_BOLD, tB_UNDERLINE, tB_REVERSE :: Word16
tB_DEFAULT, tB_BLACK, tB_RED, tB_GREEN, tB_YELLOW, tB_BLUE, tB_MAGENTA, tB_CYAN, tB_WHITE :: Word16

tB_INPUT_CURRENT = {# const TB_INPUT_CURRENT #}
tB_INPUT_ESC     = {# const TB_INPUT_ESC     #}
tB_INPUT_ALT     = {# const TB_INPUT_ALT     #}
tB_INPUT_MOUSE   = {# const TB_INPUT_MOUSE   #}

tB_HIDE_CURSOR   = {# const TB_HIDE_CURSOR #}

tB_BOLD          = {# const TB_BOLD #}
tB_UNDERLINE     = {# const TB_UNDERLINE #}
tB_REVERSE       = {# const TB_REVERSE #}

tB_DEFAULT       = {# const TB_DEFAULT #}
tB_BLACK         = {# const TB_BLACK #}
tB_RED           = {# const TB_RED #}
tB_GREEN         = {# const TB_GREEN #}
tB_YELLOW        = {# const TB_YELLOW #}
tB_BLUE          = {# const TB_BLUE #}
tB_MAGENTA       = {# const TB_MAGENTA #}
tB_CYAN          = {# const TB_CYAN #}
tB_WHITE         = {# const TB_WHITE #}

--------------------------------------------------------------------------------
-- Enums
--------------------------------------------------------------------------------

{#
  enum define TbEventType
    { TB_EVENT_KEY as TbEventTypeKey
    , TB_EVENT_RESIZE as TbEventTypeResize
    , TB_EVENT_MOUSE as TbEventTypeMouse
    }
#}

{#
  enum define TbInitResult
    { 0 as TbInitOk
    , TB_EUNSUPPORTED_TERMINAL as TbUnsupportedTerminal
    , TB_EFAILED_TO_OPEN_TTY as TbFailedToOpenTTY
    , TB_EPIPE_TRAP_ERROR as TbPipeTrapError
    }
#}

{#
  enum define TbKey
    { TB_KEY_F1 as TbKeyF1
    , TB_KEY_F2 as TbKeyF2
    , TB_KEY_F3 as TbKeyF3
    , TB_KEY_F4 as TbKeyF4
    , TB_KEY_F5 as TbKeyF5
    , TB_KEY_F6 as TbKeyF6
    , TB_KEY_F7 as TbKeyF7
    , TB_KEY_F8 as TbKeyF8
    , TB_KEY_F9 as TbKeyF9
    , TB_KEY_F10 as TbKeyF10
    , TB_KEY_F11 as TbKeyF11
    , TB_KEY_F12 as TbKeyF12
    , TB_KEY_INSERT as TbKeyInsert
    , TB_KEY_DELETE as TbKeyDelete
    , TB_KEY_HOME as TbKeyHome
    , TB_KEY_END as TbKeyEnd
    , TB_KEY_PGUP as TbKeyPageUp
    , TB_KEY_PGDN as TbKeyPageDn
    , TB_KEY_ARROW_UP as TbKeyArrowUp
    , TB_KEY_ARROW_DOWN as TbKeyArrowDown
    , TB_KEY_ARROW_LEFT as TbKeyArrowLeft
    , TB_KEY_ARROW_RIGHT as TbKeyArrowRight
    , TB_KEY_MOUSE_LEFT as TbKeyMouseLeft
    , TB_KEY_MOUSE_RIGHT as TbKeyMouseRight
    , TB_KEY_MOUSE_MIDDLE as TbKeyMouseMiddle
    , TB_KEY_MOUSE_RELEASE as TbKeyMouseRelease
    , TB_KEY_MOUSE_WHEEL_UP as TbKeyMouseWheelUp
    , TB_KEY_MOUSE_WHEEL_DOWN as TbKeyMouseWheelDown
    , TB_KEY_CTRL_TILDE as TbKeyCtrlTilde
    , TB_KEY_CTRL_2 as TbKeyCtrl2
    , TB_KEY_CTRL_A as TbKeyCtrlA
    , TB_KEY_CTRL_B as TbKeyCtrlB
    , TB_KEY_CTRL_C as TbKeyCtrlC
    , TB_KEY_CTRL_D as TbKeyCtrlD
    , TB_KEY_CTRL_E as TbKeyCtrlE
    , TB_KEY_CTRL_F as TbKeyCtrlF
    , TB_KEY_CTRL_G as TbKeyCtrlG
    , TB_KEY_BACKSPACE as TbKeyBackspace
    , TB_KEY_CTRL_H as TbKeyCtrlH
    , TB_KEY_TAB as TbKeyTab
    , TB_KEY_CTRL_I as TbKeyCtrlI
    , TB_KEY_CTRL_J as TbKeyCtrlJ
    , TB_KEY_CTRL_K as TbKeyCtrlK
    , TB_KEY_CTRL_L as TbKeyCtrlL
    , TB_KEY_ENTER as TbKeyEnter
    , TB_KEY_CTRL_M as TbKeyCtrlM
    , TB_KEY_CTRL_N as TbKeyCtrlN
    , TB_KEY_CTRL_O as TbKeyCtrlO
    , TB_KEY_CTRL_P as TbKeyCtrlP
    , TB_KEY_CTRL_Q as TbKeyCtrlQ
    , TB_KEY_CTRL_R as TbKeyCtrlR
    , TB_KEY_CTRL_S as TbKeyCtrlS
    , TB_KEY_CTRL_T as TbKeyCtrlT
    , TB_KEY_CTRL_U as TbKeyCtrlU
    , TB_KEY_CTRL_V as TbKeyCtrlV
    , TB_KEY_CTRL_W as TbKeyCtrlW
    , TB_KEY_CTRL_X as TbKeyCtrlX
    , TB_KEY_CTRL_Y as TbKeyCtrlY
    , TB_KEY_CTRL_Z as TbKeyCtrlZ
    , TB_KEY_ESC as TbKeyEsc
    , TB_KEY_CTRL_LSQ_BRACKET as TbKeyCtrlLsqBracket
    , TB_KEY_CTRL_3 as TbKeyCtrl3
    , TB_KEY_CTRL_4 as TbKeyCtrl4
    , TB_KEY_CTRL_BACKSLASH as TbKeyCtrlBackslash
    , TB_KEY_CTRL_5 as TbKeyCtrl5
    , TB_KEY_CTRL_RSQ_BRACKET as TbKeyCtrlRsqBracket
    , TB_KEY_CTRL_6 as TbKeyCtrl6
    , TB_KEY_CTRL_7 as TbKeyCtrl7
    , TB_KEY_CTRL_SLASH as TbKeyCtrlSlash
    , TB_KEY_CTRL_UNDERSCORE as TbKeyCtrlUnderscore
    , TB_KEY_SPACE as TbKeySpace
    , TB_KEY_BACKSPACE2 as TbKeyBackspace2
    , TB_KEY_CTRL_8 as TbKeyCtrl8
    } deriving (Show)
#}

{#
  enum define TbMod
    { 0 as TbModNone
    , TB_MOD_ALT as TbModAlt
    }
#}

{#
  enum define TbOutputMode
    { TB_OUTPUT_CURRENT as TbOutputModeCurrent
    , TB_OUTPUT_NORMAL as TbOutputModeNormal
    , TB_OUTPUT_256 as TbOutputMode256
    , TB_OUTPUT_216 as TbOutputMode216
    , TB_OUTPUT_GRAYSCALE as TbOutputModeGrayscale
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

data TbEvent
  = TbEvent !TbEventType TbMod TbKey Char Int Int Int Int

instance Storable TbEvent where
  sizeOf :: TbEvent -> Int
  sizeOf _ =
    {# sizeof tb_event #}

  alignment :: TbEvent -> Int
  alignment _ =
    {# alignof tb_event #}

  peek :: Ptr TbEvent -> IO TbEvent
  peek p =
    TbEvent
      <$> ((toEnum . fromIntegral) <$> {# get tb_event->type #} p)
      <*> ((toEnum . fromIntegral) <$> {# get tb_event->mod #} p)
      <*> ((toEnum . fromIntegral) <$> {# get tb_event->key #} p)
      <*> ((toEnum . fromIntegral) <$> {# get tb_event->ch #} p)
      <*> (fromIntegral <$> {# get tb_event->w #} p)
      <*> (fromIntegral <$> {# get tb_event->h #} p)
      <*> (fromIntegral <$> {# get tb_event->x #} p)
      <*> (fromIntegral <$> {# get tb_event->y #} p)

  poke :: Ptr TbEvent -> TbEvent -> IO ()
  poke p (TbEvent typ mod key ch w h x y) = do
    {# set tb_event.type #} p (fromIntegral (fromEnum typ))
    {# set tb_event.mod #} p (fromIntegral (fromEnum mod))
    {# set tb_event.key #} p (fromIntegral (fromEnum key))
    {# set tb_event.ch #} p (fromIntegral (fromEnum ch))
    {# set tb_event.w #} p (fromIntegral w)
    {# set tb_event.h #} p (fromIntegral h)
    {# set tb_event.x #} p (fromIntegral x)
    {# set tb_event.y #} p (fromIntegral y)

{# pointer *tb_event as TbEventPtr -> TbEvent #}

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

{#
  fun tb_change_cell
    { `Int', `Int', charToUInt `Char', `Word16', `Word16' } -> `()'
#}

{#
  fun tb_clear
    { } -> `()'
#}

{#
  fun tb_height
    { } -> `Int'
#}

{#
  fun tb_init
    { } -> `TbInitResult'
#}

{#
  fun tb_peek_event
    { `TbEventPtr', `Int' } -> `Int'
#}

{#
  fun tb_poll_event
    { `TbEventPtr' } -> `Int'
#}

{#
  fun tb_present
    { } -> `()'
#}

{#
  fun tb_select_input_mode
    { `Int' } -> `Int'
#}

{#
  fun tb_select_output_mode
    { `TbOutputMode' } -> `TbOutputMode'
#}

{#
  fun tb_set_clear_attributes
    { `Word16', `Word16' } -> `()'
#}

{#
  fun tb_set_cursor
    { `Int', `Int' } -> `()'
#}

{#
  fun tb_shutdown
    { } -> `()'
#}

{#
  fun tb_width
    { } -> `Int'
#}

--------------------------------------------------------------------------------
-- Misc.
--------------------------------------------------------------------------------

charToUInt :: Char -> CUInt
charToUInt =
  fromIntegral . ord

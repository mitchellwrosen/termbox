{-# LANGUAGE PatternSynonyms #-}

module Termbox.Key
  ( Key (..),
    parseKey,
    pattern KeyCtrlH,
    pattern KeyCtrlLsqBracket,
    pattern KeyCtrl2,
    pattern KeyCtrl3,
    pattern KeyCtrl4,
    pattern KeyCtrl5,
    pattern KeyCtrl7,
    pattern KeyCtrlM,
    pattern KeyCtrlI,
    pattern KeyCtrlUnderscore,
  )
where

import Data.Word (Word16)
import Termbox.Internal

-- | A key event.
data Key
  = KeyChar Char
  | KeyArrowDown
  | KeyArrowLeft
  | KeyArrowRight
  | KeyArrowUp
  | KeyBackspace
  | -- | Also: @Ctrl+H@
    KeyCtrlBackspace
  | KeyCtrl6
  | KeyCtrl8
  | KeyCtrlA
  | KeyCtrlB
  | -- | Also: @Ctrl-4@
    KeyCtrlBackslash
  | KeyCtrlC
  | KeyCtrlD
  | KeyCtrlE
  | KeyCtrlF
  | KeyCtrlG
  | KeyCtrlJ
  | KeyCtrlK
  | KeyCtrlL
  | KeyCtrlN
  | KeyCtrlO
  | KeyCtrlP
  | KeyCtrlQ
  | KeyCtrlR
  | -- | Also: @Ctrl-5@
    KeyCtrlRsqBracket
  | KeyCtrlS
  | -- | Also: @Ctrl-/@, @Ctrl-_@
    KeyCtrlSlash
  | -- | Also: @Ctrl+2@
    KeyCtrlTilde
  | KeyCtrlT
  | KeyCtrlU
  | KeyCtrlV
  | KeyCtrlW
  | KeyCtrlX
  | KeyCtrlY
  | KeyCtrlZ
  | KeyDelete
  | KeyEnd
  | -- | Also: @Ctrl-M@
    KeyEnter
  | -- | Also: @Ctrl-[@, @Ctrl-3@
    KeyEsc
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
  | -- | Also: @Ctrl+I@
    KeyTab
  deriving (Eq, Ord, Show)

pattern KeyCtrlH :: Key
pattern KeyCtrlH = KeyCtrlBackspace

pattern KeyCtrlLsqBracket :: Key
pattern KeyCtrlLsqBracket = KeyEsc

pattern KeyCtrl2 :: Key
pattern KeyCtrl2 = KeyCtrlTilde

pattern KeyCtrl3 :: Key
pattern KeyCtrl3 = KeyEsc

pattern KeyCtrl4 :: Key
pattern KeyCtrl4 = KeyCtrlBackslash

pattern KeyCtrl5 :: Key
pattern KeyCtrl5 = KeyCtrlRsqBracket

pattern KeyCtrl7 :: Key
pattern KeyCtrl7 = KeyCtrlSlash

pattern KeyCtrlM :: Key
pattern KeyCtrlM = KeyEnter

pattern KeyCtrlI :: Key
pattern KeyCtrlI = KeyTab

pattern KeyCtrlUnderscore :: Key
pattern KeyCtrlUnderscore = KeyCtrlSlash

parseKey :: Word16 -> Key
parseKey key
  | key == tB_KEY_ARROW_DOWN = KeyArrowDown
  | key == tB_KEY_ARROW_LEFT = KeyArrowLeft
  | key == tB_KEY_ARROW_RIGHT = KeyArrowRight
  | key == tB_KEY_ARROW_UP = KeyArrowUp
  | key == tB_KEY_BACKSPACE = KeyBackspace
  | key == tB_KEY_CTRL_TILDE = KeyCtrlTilde
  | key == tB_KEY_CTRL_6 = KeyCtrl6
  | key == tB_KEY_CTRL_8 = KeyCtrl8
  | key == tB_KEY_CTRL_A = KeyCtrlA
  | key == tB_KEY_CTRL_B = KeyCtrlB
  | key == tB_KEY_CTRL_BACKSLASH = KeyCtrlBackslash
  | key == tB_KEY_CTRL_C = KeyCtrlC
  | key == tB_KEY_CTRL_D = KeyCtrlD
  | key == tB_KEY_CTRL_E = KeyCtrlE
  | key == tB_KEY_CTRL_F = KeyCtrlF
  | key == tB_KEY_CTRL_G = KeyCtrlG
  | key == tB_KEY_CTRL_H = KeyCtrlBackspace
  | key == tB_KEY_CTRL_J = KeyCtrlJ
  | key == tB_KEY_CTRL_K = KeyCtrlK
  | key == tB_KEY_CTRL_L = KeyCtrlL
  | key == tB_KEY_CTRL_N = KeyCtrlN
  | key == tB_KEY_CTRL_O = KeyCtrlO
  | key == tB_KEY_CTRL_P = KeyCtrlP
  | key == tB_KEY_CTRL_Q = KeyCtrlQ
  | key == tB_KEY_CTRL_R = KeyCtrlR
  | key == tB_KEY_CTRL_RSQ_BRACKET = KeyCtrlRsqBracket
  | key == tB_KEY_CTRL_S = KeyCtrlS
  | key == tB_KEY_CTRL_SLASH = KeyCtrlSlash
  | key == tB_KEY_CTRL_T = KeyCtrlT
  | key == tB_KEY_CTRL_U = KeyCtrlU
  | key == tB_KEY_CTRL_V = KeyCtrlV
  | key == tB_KEY_CTRL_W = KeyCtrlW
  | key == tB_KEY_CTRL_X = KeyCtrlX
  | key == tB_KEY_CTRL_Y = KeyCtrlY
  | key == tB_KEY_CTRL_Z = KeyCtrlZ
  | key == tB_KEY_DELETE = KeyDelete
  | key == tB_KEY_END = KeyEnd
  | key == tB_KEY_ENTER = KeyEnter
  | key == tB_KEY_ESC = KeyEsc
  | key == tB_KEY_F1 = KeyF1
  | key == tB_KEY_F10 = KeyF10
  | key == tB_KEY_F11 = KeyF11
  | key == tB_KEY_F12 = KeyF12
  | key == tB_KEY_F2 = KeyF2
  | key == tB_KEY_F3 = KeyF3
  | key == tB_KEY_F4 = KeyF4
  | key == tB_KEY_F5 = KeyF5
  | key == tB_KEY_F6 = KeyF6
  | key == tB_KEY_F7 = KeyF7
  | key == tB_KEY_F8 = KeyF8
  | key == tB_KEY_F9 = KeyF9
  | key == tB_KEY_HOME = KeyHome
  | key == tB_KEY_INSERT = KeyInsert
  | key == tB_KEY_PGDN = KeyPageDn
  | key == tB_KEY_PGUP = KeyPageUp
  | key == tB_KEY_SPACE = KeySpace
  | key == tB_KEY_TAB = KeyTab
  | otherwise = error ("termbox: unknown key " ++ show key)

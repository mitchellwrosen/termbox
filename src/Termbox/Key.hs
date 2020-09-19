module Termbox.Key
  ( Key (..),
    parseKey,
  )
where

import qualified Termbox.C

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

-- | Parse a 'Key' from a 'Termbox.C.Key'.
parseKey :: Termbox.C.Key -> Key
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
  key -> error ("termbox: unknown key " ++ show key)

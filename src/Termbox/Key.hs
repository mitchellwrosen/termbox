module Termbox.Key
  ( Key (..),
    parseKey,
  )
where

import Termbox.C (TbKey (..))

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

-- | Parse a 'Key' from a 'TbKey'.
parseKey :: TbKey -> Key
parseKey = \case
  TbKeyArrowDown -> KeyArrowDown
  TbKeyArrowLeft -> KeyArrowLeft
  TbKeyArrowRight -> KeyArrowRight
  TbKeyArrowUp -> KeyArrowUp
  TbKeyBackspace -> KeyBackspace
  TbKeyBackspace2 -> KeyBackspace2
  TbKeyCtrl2 -> KeyCtrl2
  TbKeyCtrl3 -> KeyCtrl3
  TbKeyCtrl4 -> KeyCtrl4
  TbKeyCtrl5 -> KeyCtrl5
  TbKeyCtrl6 -> KeyCtrl6
  TbKeyCtrl7 -> KeyCtrl7
  TbKeyCtrl8 -> KeyCtrl8
  TbKeyCtrlA -> KeyCtrlA
  TbKeyCtrlB -> KeyCtrlB
  TbKeyCtrlBackslash -> KeyCtrlBackslash
  TbKeyCtrlC -> KeyCtrlC
  TbKeyCtrlD -> KeyCtrlD
  TbKeyCtrlE -> KeyCtrlE
  TbKeyCtrlF -> KeyCtrlF
  TbKeyCtrlG -> KeyCtrlG
  TbKeyCtrlH -> KeyCtrlH
  TbKeyCtrlI -> KeyCtrlI
  TbKeyCtrlJ -> KeyCtrlJ
  TbKeyCtrlK -> KeyCtrlK
  TbKeyCtrlL -> KeyCtrlL
  TbKeyCtrlLsqBracket -> KeyCtrlLsqBracket
  TbKeyCtrlM -> KeyCtrlM
  TbKeyCtrlN -> KeyCtrlN
  TbKeyCtrlO -> KeyCtrlO
  TbKeyCtrlP -> KeyCtrlP
  TbKeyCtrlQ -> KeyCtrlQ
  TbKeyCtrlR -> KeyCtrlR
  TbKeyCtrlRsqBracket -> KeyCtrlRsqBracket
  TbKeyCtrlS -> KeyCtrlS
  TbKeyCtrlSlash -> KeyCtrlSlash
  TbKeyCtrlT -> KeyCtrlT
  TbKeyCtrlTilde -> KeyCtrlTilde
  TbKeyCtrlU -> KeyCtrlU
  TbKeyCtrlUnderscore -> KeyCtrlUnderscore
  TbKeyCtrlV -> KeyCtrlV
  TbKeyCtrlW -> KeyCtrlW
  TbKeyCtrlX -> KeyCtrlX
  TbKeyCtrlY -> KeyCtrlY
  TbKeyCtrlZ -> KeyCtrlZ
  TbKeyDelete -> KeyDelete
  TbKeyEnd -> KeyEnd
  TbKeyEnter -> KeyEnter
  TbKeyEsc -> KeyEsc
  TbKeyF1 -> KeyF1
  TbKeyF10 -> KeyF10
  TbKeyF11 -> KeyF11
  TbKeyF12 -> KeyF12
  TbKeyF2 -> KeyF2
  TbKeyF3 -> KeyF3
  TbKeyF4 -> KeyF4
  TbKeyF5 -> KeyF5
  TbKeyF6 -> KeyF6
  TbKeyF7 -> KeyF7
  TbKeyF8 -> KeyF8
  TbKeyF9 -> KeyF9
  TbKeyHome -> KeyHome
  TbKeyInsert -> KeyInsert
  TbKeyPageDn -> KeyPageDn
  TbKeyPageUp -> KeyPageUp
  TbKeySpace -> KeySpace
  TbKeyTab -> KeyTab
  key -> error ("termbox: unknown key " ++ show key)

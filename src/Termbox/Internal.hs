module Termbox.Internal where

import Data.Int (Int32)
import Data.Word
import Foreign.Ptr (Ptr)
import Foreign.Storable
import Prelude hiding (mod)

tB_EUNSUPPORTED_TERMINAL, tB_EFAILED_TO_OPEN_TTY, tB_EPIPE_TRAP_ERROR :: Int
tB_EUNSUPPORTED_TERMINAL = -1
tB_EFAILED_TO_OPEN_TTY = -2
tB_EPIPE_TRAP_ERROR = -3

tB_INPUT_CURRENT, tB_INPUT_ESC, tB_INPUT_ALT, tB_INPUT_MOUSE :: Int
tB_INPUT_CURRENT = 0
tB_INPUT_ESC = 1
tB_INPUT_ALT = 2
tB_INPUT_MOUSE = 4

tB_OUTPUT_CURRENT, tB_OUTPUT_NORMAL, tB_OUTPUT_256, tB_OUTPUT_216, tB_OUTPUT_GRAYSCALE :: Int
tB_OUTPUT_CURRENT = 0
tB_OUTPUT_NORMAL = 1
tB_OUTPUT_256 = 2
tB_OUTPUT_216 = 3
tB_OUTPUT_GRAYSCALE = 4

tB_EVENT_KEY, tB_EVENT_RESIZE, tB_EVENT_MOUSE :: Word8
tB_EVENT_KEY = 1
tB_EVENT_RESIZE = 2
tB_EVENT_MOUSE = 3

tB_MOD_ALT :: Word8
tB_MOD_ALT = 0x01

-- #define TB_MOD_MOTION 0x02

tB_HIDE_CURSOR :: Int
tB_HIDE_CURSOR = -1

tB_BOLD, tB_UNDERLINE, tB_REVERSE :: Word16
tB_BOLD = 0x0100
tB_UNDERLINE = 0x0200
tB_REVERSE = 0x0400

tB_DEFAULT, tB_BLACK, tB_RED, tB_GREEN, tB_YELLOW, tB_BLUE, tB_MAGENTA, tB_CYAN, tB_WHITE :: Word16
tB_DEFAULT = 0x00
tB_BLACK = 0x01
tB_RED = 0x02
tB_GREEN = 0x03
tB_YELLOW = 0x04
tB_BLUE = 0x05
tB_MAGENTA = 0x06
tB_CYAN = 0x07
tB_WHITE = 0x08

tB_KEY_F1 :: Word16
tB_KEY_F1 = 0xFFFF - 0

tB_KEY_F2 :: Word16
tB_KEY_F2 = 0xFFFF - 1

tB_KEY_F3 :: Word16
tB_KEY_F3 = 0xFFFF - 2

tB_KEY_F4 :: Word16
tB_KEY_F4 = 0xFFFF - 3

tB_KEY_F5 :: Word16
tB_KEY_F5 = 0xFFFF - 4

tB_KEY_F6 :: Word16
tB_KEY_F6 = 0xFFFF - 5

tB_KEY_F7 :: Word16
tB_KEY_F7 = 0xFFFF - 6

tB_KEY_F8 :: Word16
tB_KEY_F8 = 0xFFFF - 7

tB_KEY_F9 :: Word16
tB_KEY_F9 = 0xFFFF - 8

tB_KEY_F10 :: Word16
tB_KEY_F10 = 0xFFFF - 9

tB_KEY_F11 :: Word16
tB_KEY_F11 = 0xFFFF - 10

tB_KEY_F12 :: Word16
tB_KEY_F12 = 0xFFFF - 11

tB_KEY_INSERT :: Word16
tB_KEY_INSERT = 0xFFFF - 12

tB_KEY_DELETE :: Word16
tB_KEY_DELETE = 0xFFFF - 13

tB_KEY_HOME :: Word16
tB_KEY_HOME = 0xFFFF - 14

tB_KEY_END :: Word16
tB_KEY_END = 0xFFFF - 15

tB_KEY_PGUP :: Word16
tB_KEY_PGUP = 0xFFFF - 16

tB_KEY_PGDN :: Word16
tB_KEY_PGDN = 0xFFFF - 17

tB_KEY_ARROW_UP :: Word16
tB_KEY_ARROW_UP = 0xFFFF - 18

tB_KEY_ARROW_DOWN :: Word16
tB_KEY_ARROW_DOWN = 0xFFFF - 19

tB_KEY_ARROW_LEFT :: Word16
tB_KEY_ARROW_LEFT = 0xFFFF - 20

tB_KEY_ARROW_RIGHT :: Word16
tB_KEY_ARROW_RIGHT = 0xFFFF - 21

tB_KEY_MOUSE_LEFT :: Word16
tB_KEY_MOUSE_LEFT = 0xFFFF - 22

tB_KEY_MOUSE_RIGHT :: Word16
tB_KEY_MOUSE_RIGHT = 0xFFFF - 23

tB_KEY_MOUSE_MIDDLE :: Word16
tB_KEY_MOUSE_MIDDLE = 0xFFFF - 24

tB_KEY_MOUSE_RELEASE :: Word16
tB_KEY_MOUSE_RELEASE = 0xFFFF - 25

tB_KEY_MOUSE_WHEEL_UP :: Word16
tB_KEY_MOUSE_WHEEL_UP = 0xFFFF - 26

tB_KEY_MOUSE_WHEEL_DOWN :: Word16
tB_KEY_MOUSE_WHEEL_DOWN = 0xFFFF - 27

tB_KEY_CTRL_TILDE :: Word16
tB_KEY_CTRL_TILDE = 0x00

tB_KEY_CTRL_2 :: Word16
tB_KEY_CTRL_2 = 0x00

tB_KEY_CTRL_A :: Word16
tB_KEY_CTRL_A = 0x01

tB_KEY_CTRL_B :: Word16
tB_KEY_CTRL_B = 0x02

tB_KEY_CTRL_C :: Word16
tB_KEY_CTRL_C = 0x03

tB_KEY_CTRL_D :: Word16
tB_KEY_CTRL_D = 0x04

tB_KEY_CTRL_E :: Word16
tB_KEY_CTRL_E = 0x05

tB_KEY_CTRL_F :: Word16
tB_KEY_CTRL_F = 0x06

tB_KEY_CTRL_G :: Word16
tB_KEY_CTRL_G = 0x07

tB_KEY_BACKSPACE :: Word16
tB_KEY_BACKSPACE = 0x08

tB_KEY_CTRL_H :: Word16
tB_KEY_CTRL_H = 0x08

tB_KEY_TAB :: Word16
tB_KEY_TAB = 0x09

tB_KEY_CTRL_I :: Word16
tB_KEY_CTRL_I = 0x09

tB_KEY_CTRL_J :: Word16
tB_KEY_CTRL_J = 0x0A

tB_KEY_CTRL_K :: Word16
tB_KEY_CTRL_K = 0x0B

tB_KEY_CTRL_L :: Word16
tB_KEY_CTRL_L = 0x0C

tB_KEY_ENTER :: Word16
tB_KEY_ENTER = 0x0D

tB_KEY_CTRL_M :: Word16
tB_KEY_CTRL_M = 0x0D

tB_KEY_CTRL_N :: Word16
tB_KEY_CTRL_N = 0x0E

tB_KEY_CTRL_O :: Word16
tB_KEY_CTRL_O = 0x0F

tB_KEY_CTRL_P :: Word16
tB_KEY_CTRL_P = 0x10

tB_KEY_CTRL_Q :: Word16
tB_KEY_CTRL_Q = 0x11

tB_KEY_CTRL_R :: Word16
tB_KEY_CTRL_R = 0x12

tB_KEY_CTRL_S :: Word16
tB_KEY_CTRL_S = 0x13

tB_KEY_CTRL_T :: Word16
tB_KEY_CTRL_T = 0x14

tB_KEY_CTRL_U :: Word16
tB_KEY_CTRL_U = 0x15

tB_KEY_CTRL_V :: Word16
tB_KEY_CTRL_V = 0x16

tB_KEY_CTRL_W :: Word16
tB_KEY_CTRL_W = 0x17

tB_KEY_CTRL_X :: Word16
tB_KEY_CTRL_X = 0x18

tB_KEY_CTRL_Y :: Word16
tB_KEY_CTRL_Y = 0x19

tB_KEY_CTRL_Z :: Word16
tB_KEY_CTRL_Z = 0x1A

tB_KEY_ESC :: Word16
tB_KEY_ESC = 0x1B

tB_KEY_CTRL_LSQ_BRACKET :: Word16
tB_KEY_CTRL_LSQ_BRACKET = 0x1B

tB_KEY_CTRL_3 :: Word16
tB_KEY_CTRL_3 = 0x1B

tB_KEY_CTRL_4 :: Word16
tB_KEY_CTRL_4 = 0x1C

tB_KEY_CTRL_BACKSLASH :: Word16
tB_KEY_CTRL_BACKSLASH = 0x1C

tB_KEY_CTRL_5 :: Word16
tB_KEY_CTRL_5 = 0x1D

tB_KEY_CTRL_RSQ_BRACKET :: Word16
tB_KEY_CTRL_RSQ_BRACKET = 0x1D

tB_KEY_CTRL_6 :: Word16
tB_KEY_CTRL_6 = 0x1E

tB_KEY_CTRL_7 :: Word16
tB_KEY_CTRL_7 = 0x1F

tB_KEY_CTRL_SLASH :: Word16
tB_KEY_CTRL_SLASH = 0x1F

tB_KEY_CTRL_UNDERSCORE :: Word16
tB_KEY_CTRL_UNDERSCORE = 0x1F

tB_KEY_SPACE :: Word16
tB_KEY_SPACE = 0x20

tB_KEY_BACKSPACE2 :: Word16
tB_KEY_BACKSPACE2 = 0x7F

tB_KEY_CTRL_8 :: Word16
tB_KEY_CTRL_8 = 0x7F

data TbEvent
  = TbEvent !Word8 Word8 Word16 Word32 Int32 Int32 Int32 Int32

instance Storable TbEvent where
  sizeOf :: TbEvent -> Int
  sizeOf _ =
    24

  alignment :: TbEvent -> Int
  alignment _ =
    4

  peek :: Ptr TbEvent -> IO TbEvent
  peek ptr =
    TbEvent
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 1
      <*> peekByteOff ptr 2
      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 12
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 20

  poke :: Ptr TbEvent -> TbEvent -> IO ()
  poke ptr (TbEvent typ mod key ch w h x y) = do
    pokeByteOff ptr 0 typ
    pokeByteOff ptr 1 mod
    pokeByteOff ptr 2 key
    pokeByteOff ptr 4 ch
    pokeByteOff ptr 8 w
    pokeByteOff ptr 12 h
    pokeByteOff ptr 16 x
    pokeByteOff ptr 20 y

foreign import ccall unsafe "tb_change_cell"
  tb_change_cell :: Int -> Int -> Word32 -> Word16 -> Word16 -> IO ()

foreign import ccall unsafe "tb_clear"
  tb_clear :: IO ()

foreign import ccall unsafe "tb_height"
  tb_height :: IO Int

foreign import ccall unsafe "tb_init"
  tb_init :: IO Int

foreign import ccall safe "tb_peek_event"
  tb_peek_event :: Ptr TbEvent -> Int -> IO Int

foreign import ccall safe "tb_poll_event"
  tb_poll_event :: Ptr TbEvent -> IO Int

foreign import ccall unsafe "tb_present"
  tb_present :: IO ()

foreign import ccall unsafe "tb_select_input_mode"
  tb_select_input_mode :: Int -> IO Int

foreign import ccall unsafe "tb_select_output_mode"
  tb_select_output_mode :: Int -> IO Int

foreign import ccall unsafe "tb_set_clear_attributes"
  tb_set_clear_attributes :: Word16 -> Word16 -> IO ()

foreign import ccall unsafe "tb_set_cursor"
  tb_set_cursor :: Int -> Int -> IO ()

foreign import ccall unsafe "tb_shutdown"
  tb_shutdown :: IO ()

foreign import ccall unsafe "tb_width"
  tb_width :: IO Int

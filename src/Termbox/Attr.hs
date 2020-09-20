module Termbox.Attr
  ( Attr,
    black,
    blue,
    bold,
    cyan,
    green,
    magenta,
    red,
    reverse,
    underline,
    white,
    yellow,
    --
    attrToWord,
    wordToAttr,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.Semigroup (Semigroup (..))
import Data.Word (Word16)
import Termbox.Internal
import Prelude hiding (reverse)

-- | A cell attribute, which includes its color, and whether or not it is
-- bold, underlined, and/or reversed.
--
-- A cell can only have one color, but may be (for example) bold /and/
-- underlined. The 'Monoid' instance combines 'Attr's this way, with a right bias.
data Attr
  = Attr !Word16 {- color -} !Word16 {- attr -}
  deriving (Eq, Show)

instance Monoid Attr where
  mempty :: Attr
  mempty =
    Attr tB_DEFAULT 0

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

-- | Right-biased color; attributes are merged.
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
  Attr tB_BLACK 0

-- | @red = 2@.
red :: Attr
red =
  Attr tB_RED 0

-- | @green = 3@.
green :: Attr
green =
  Attr tB_GREEN 0

-- | @yellow = 4@.
yellow :: Attr
yellow =
  Attr tB_YELLOW 0

-- | @blue = 5@.
blue :: Attr
blue =
  Attr tB_BLUE 0

-- | @magenta = 6@.
magenta :: Attr
magenta =
  Attr tB_MAGENTA 0

-- | @cyan = 7@.
cyan :: Attr
cyan =
  Attr tB_CYAN 0

-- | @white = 8@.
white :: Attr
white =
  Attr tB_WHITE 0

-- | Bold modifier attribute.
bold :: Attr
bold =
  Attr tB_DEFAULT tB_BOLD

-- | Underline modifier attribute.
underline :: Attr
underline =
  Attr tB_DEFAULT tB_UNDERLINE

-- | Reverse modifier attribute.
reverse :: Attr
reverse =
  Attr tB_DEFAULT tB_REVERSE

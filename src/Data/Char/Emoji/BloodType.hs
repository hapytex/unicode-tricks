{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Emoji.BloodType
-- Description : A module that defines the emoji for /blood types/.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The Unicode standard defines four emoji for the 'O', 'B', 'A', and 'AB' blood type.
module Data.Char.Emoji.BloodType
  ( -- * Blood type emoji
    BloodType (O, B, A, AB),

    -- * Drop of blood emoji
    pattern DropOfBlood,
  )
where

import Control.DeepSeq (NFData)
import Data.Bits (Bits (bit, bitSize, bitSizeMaybe, complement, isSigned, popCount, rotate, shift, testBit, xor, (.&.), (.|.)))
import Data.Char.Core (UnicodeText (fromUnicodeText, isInTextRange, toUnicodeText))
import Data.Char.Emoji.Core (pattern EmojiSuffix)
import Data.Data (Data)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)

-- | A emoji that depicts a drop of blood. This looks like 🩸.
pattern DropOfBlood :: Char
pattern DropOfBlood = '\x1fa78'

-- | A 'BloodType' object used to convert to its unicode equivalent. The
-- 'BloodType' is also seen as a 2-bit value with the leftmost bit representing
-- the presence of /A antigens/ and the rightmost the presence of /B antigens/.
data BloodType
  = -- | The /O blood type/, with no presence of A and B antigens, denoted by 🅾️.
    O
  | -- | The /B blood type/, with presence of the B antigen, denoted by 🅱️.
    B
  | -- | The /A blood type/, with presence of the A antigen, denoted by 🅰️.
    A
  | -- | The /AB blood type/, with presence of the A and B antigens, denoted by 🆎.
    AB
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary BloodType where
  arbitrary = arbitraryBoundedEnum

_overEnumMask :: Enum a => Int -> (Int -> Int) -> a -> a
_overEnumMask m f = toEnum . (m .&.) . f . fromEnum

_overEnum2 :: Enum a => (Int -> Int -> Int) -> a -> a -> a
_overEnum2 f x y = toEnum (on f fromEnum x y)

_overEnumMask2 :: Enum a => Int -> (Int -> Int -> Int) -> a -> a -> a
_overEnumMask2 m f x y = toEnum (m .&. on f fromEnum x y)

instance Bits BloodType where
  (.&.) = _overEnum2 (.&.)
  (.|.) = _overEnum2 (.|.)
  xor = _overEnumMask2 0x03 xor
  complement O = AB
  complement A = B
  complement B = A
  complement AB = O
  shift abo n = _overEnumMask 0x03 (`shift` n) abo
  rotate = flip (go . (0x01 .&.))
    where
      go 1 A = B
      go 1 B = B
      go _ x = x
  bitSize = const 2
  bitSizeMaybe = const (Just 2)
  isSigned = const False
  testBit = testBit . fromEnum
  bit 0 = B
  bit 1 = A
  bit _ = O
  popCount O = 0
  popCount A = 1
  popCount B = 1
  popCount AB = 2

instance Hashable BloodType

instance NFData BloodType

instance UnicodeText BloodType where
  toUnicodeText AB = "\x1f18e"
  toUnicodeText A = "\x1f170\xfe0f"
  toUnicodeText B = "\x1f171\xfe0f"
  toUnicodeText O = "\x1f17e\xfe0f"
  fromUnicodeText "\x1f18e" = Just AB
  fromUnicodeText t
    | [c, EmojiSuffix] <- unpack t = go c
    | otherwise = Nothing
    where
      go '\x1f170' = Just A
      go '\x1f171' = Just B
      go '\x1f17e' = Just O
      go _ = Nothing
  isInTextRange "\x1f170\xfe0f" = True
  isInTextRange "\x1f171\xfe0f" = True
  isInTextRange "\x1f17e\xfe0f" = True
  isInTextRange "\x1f18e" = True
  isInTextRange _ = False

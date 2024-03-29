{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.Char.Braille
-- Description : A module used to render Braille characters in unicode.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Unicode has a Braille segment for Braille with six dot cells, and a segment for Braille with eight dot cells, this module aims to make it more convenient to render such characters.
module Data.Char.Braille
  ( -- * Datastructures to store the state of the Braille character.
    Braille6 (Braille6, top, middle, bottom),
    Braille (Braille, row1, row2, row3, row4),

    -- * Converting 'Braille6' to 'Braille'
    toBraille,
    toBraille',

    -- * Rendering Braille characters.
    braille6,
    braille,

    -- * Converting a character to 'Braille'
    fromBraille6,
    fromBraille6',
    fromBraille,
    fromBraille',
  )
where

import Control.DeepSeq (NFData, NFData1)
import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Bool (bool)
import Data.Char (chr, ord)
import Data.Char.Block (Row (Row))
import Data.Char.Core (MirrorHorizontal (mirrorHorizontal), MirrorVertical (mirrorVertical), UnicodeCharacter (fromUnicodeChar, fromUnicodeChar', isInCharRange, toUnicodeChar), UnicodeText (isInTextRange), generateIsInTextRange')
import Data.Data (Data)
import Data.Functor.Classes (Eq1 (liftEq), Ord1 (liftCompare))
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif

import GHC.Generics (Generic, Generic1)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), Arbitrary1 (liftArbitrary), arbitrary1)

-- | A datastructure to render Braille patterns with six dots cells.
data Braille6 a = Braille6
  { -- | The state of the top row of the Braille character.
    top :: Row a,
    -- | The state of the middle row of the Braille character.
    middle :: Row a,
    -- | The state of the bottom row of the Braille character.
    bottom :: Row a
  }
  deriving (Bounded, Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance Eq1 Braille6 where
  liftEq cmp ~(Braille6 ta ma ba) ~(Braille6 tb mb bb) = cmp' ta tb && cmp' ma mb && cmp' ba bb
    where
      cmp' = liftEq cmp

instance Hashable1 Braille6

instance Hashable a => Hashable (Braille6 a)

instance MirrorHorizontal (Braille6 a) where
  mirrorHorizontal (Braille6 a b c) = Braille6 c b a

instance MirrorVertical (Braille6 a) where
  mirrorVertical (Braille6 a b c) = Braille6 (mirrorVertical a) (mirrorVertical b) (mirrorVertical c)

instance NFData a => NFData (Braille6 a)

instance NFData1 Braille6

instance Ord1 Braille6 where
  liftCompare cmp ~(Braille6 ta ma ba) ~(Braille6 tb mb bb) = cmp' ta tb <> cmp' ma mb <> cmp' ba bb
    where
      cmp' = liftCompare cmp

-- | A datastructure to render Braille patterns with eight dots cells.
data Braille a = Braille
  { -- | The state of the top row of the Braille character.
    row1 :: Row a,
    -- | The state of the second row of the Braille character.
    row2 :: Row a,
    -- | The state of the third row of the Braille character.
    row3 :: Row a,
    -- | The state of the bottom row of the Braille character.
    row4 :: Row a
  }
  deriving (Bounded, Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance Eq1 Braille where
  liftEq cmp ~(Braille a1 a2 a3 a4) ~(Braille b1 b2 b3 b4) = cmp' a1 b1 && cmp' a2 b2 && cmp' a3 b3 && cmp' a4 b4
    where
      cmp' = liftEq cmp

instance Hashable1 Braille

instance Hashable a => Hashable (Braille a)

instance MirrorHorizontal (Braille a) where
  mirrorHorizontal (Braille a b c d) = Braille d c b a

instance MirrorVertical (Braille a) where
  mirrorVertical (Braille a b c d) = Braille (mirrorVertical a) (mirrorVertical b) (mirrorVertical c) (mirrorVertical d)

instance NFData a => NFData (Braille a)

instance NFData1 Braille

instance Ord1 Braille where
  liftCompare cmp ~(Braille a1 a2 a3 a4) ~(Braille b1 b2 b3 b4) = cmp' a1 b1 <> cmp' a2 b2 <> cmp' a3 b3 <> cmp' a4 b4
    where
      cmp' = liftCompare cmp

-- | Convert a 'Braille6' value to a 'Braille' character, by putting in a given
-- value at the two values at the bottom row.
toBraille' ::
  -- | The value to put in the cells of the bottom row.
  a ->
  -- | The given 'Braille6' value to convert.
  Braille6 a ->
  -- | A 'Braille' value that uses as bottom two values given as first parameter.
  Braille a
toBraille' d (Braille6 r0 r1 r2) = Braille r0 r1 r2 (Row d d)

-- | Convert a 'Braille6' value to a 'Braille6' character by setting the bottom
-- row with two 'False' values.
toBraille ::
  -- | The given 'Braille6' value to convert.
  Braille6 Bool ->
  -- | A 'Braille' value that uses as bottom two times 'False'.
  Braille Bool
toBraille = toBraille' False

-- | Convert the given 'Char'acter to a 'Braille' object of 'Bool's. If the
-- given character is not a /Braille/ character, the result is unspecified.
fromBraille' ::
  -- | The given 'Char'acter to convert.
  Char ->
  -- | The corresponding 'Braille' object of 'Bool's.
  Braille Bool
fromBraille' c = Braille (go 0x00) (go 0x01) (go 0x02) (Row (tB' 0x03) (tB' 0x04))
  where
    b = ord c .&. 0xff
    tB = testBit b
    tB' = testBit (shiftR b 3)
    go n = Row (tB n) (tB' n)

-- | Convert the given 'Char'acter to a 'Braille6' object of 'Bool's. If the
-- given character is not a /Braille/ character, or a /Braille/ character where
-- the lowest row contains filled dots, then the result is unspecified.
fromBraille6' ::
  -- | The given 'Char'acter to convert.
  Char ->
  -- | The corresponding 'Braille6' object of 'Bool's.
  Braille6 Bool
fromBraille6' c = Braille6 (go 0x00) (go 0x01) (go 0x02)
  where
    b = ord c .&. 0x3f
    go n = Row (testBit b n) (testBit (shiftR b 3) n)

-- | Convert the given 'Char'acter to a 'Braille' object of 'Bool's wrapped in
-- a 'Just'. If the given character is not a /Braille/ character, 'Nothing' is
-- returned.
fromBraille ::
  -- | The given 'Char'acter to convert.
  Char ->
  -- | The equivalent 'Braille6' object of 'Bool's wrapped in a 'Just' if it exists; 'Nothing' otherwise.
  Maybe (Braille Bool)
fromBraille c
  | '\x2800' > c || c > '\x28ff' = Nothing
  | otherwise = Just (fromBraille' c)

-- | Convert the given 'Char'acter to a 'Braille6' object of 'Bool's wrapped in
-- a 'Just'. If the given character is not a /Braille/ character, or a /Braille/
-- character where the lowest row contains filled dots, 'Nothing' is returned.
fromBraille6 ::
  -- | The given 'Char'acter to convert.
  Char ->
  -- | The equivalent 'Braille6' object of 'Bool's wrapped in a 'Just' if it exists; 'Nothing' otherwise.
  Maybe (Braille6 Bool)
fromBraille6 c
  | '\x2800' > c || c > '\x283f' = Nothing
  | otherwise = Just (fromBraille6' c)

instance Arbitrary a => Arbitrary (Braille6 a) where
  arbitrary = arbitrary1

instance Arbitrary1 Braille6 where
  liftArbitrary arb = Braille6 <$> arb' <*> arb' <*> arb'
    where
      arb' = liftArbitrary arb

instance Arbitrary a => Arbitrary (Braille a) where
  arbitrary = arbitrary1

instance Arbitrary1 Braille where
  liftArbitrary arb = Braille <$> arb' <*> arb' <*> arb' <*> arb'
    where
      arb' = liftArbitrary arb

_rowValue' :: Int -> Row Bool -> Int
_rowValue' d (Row b0 b1) = bool 0 1 b0 .|. bool 0 d b1

_rowValue :: Row Bool -> Int
_rowValue = _rowValue' 8

-- | Convert the given 'Braille6' value to a unicode character representing this
-- Braille value.
braille6 :: Braille6 Bool -> Char
braille6 = braille . toBraille

-- | Convert the given 'Braille' value to a unicode character representing this
-- braille value.
braille :: Braille Bool -> Char
braille (Braille r1 r2 r3 r4) = chr (0x2800 .|. _rowValue r1 .|. shiftL (_rowValue r2) 1 .|. shiftL (_rowValue r3) 2 .|. shiftL (_rowValue' 2 r4) 6)

instance UnicodeCharacter (Braille Bool) where
  toUnicodeChar = braille
  fromUnicodeChar = fromBraille
  fromUnicodeChar' = fromBraille'
  isInCharRange c = '\x2800' <= c && c <= '\x28ff'

instance UnicodeCharacter (Braille6 Bool) where
  toUnicodeChar = braille6
  fromUnicodeChar = fromBraille6
  fromUnicodeChar' = fromBraille6'
  isInCharRange c = '\x2800' <= c && c <= '\x283f'

instance UnicodeText (Braille Bool) where
  isInTextRange = generateIsInTextRange' @(Braille Bool)

instance UnicodeText (Braille6 Bool) where
  isInTextRange = generateIsInTextRange' @(Braille6 Bool)

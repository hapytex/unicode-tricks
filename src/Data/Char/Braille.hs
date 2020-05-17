{-# LANGUAGE DeriveTraversable, Safe #-}

{-|
Module      : Data.Char.Braille
Description : A module used to render Braille characters in unicode.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has a Braille segment for Braille with six dot cells, and a segment for Braille with eight dot cells, this module aims to make it more convenient to render such characters.
-}

module Data.Char.Braille(
    -- * Datastructures to store the state of the Braille character.
    Braille6(Braille6, top, middle, bottom)
  , Braille(Braille, row1, row2, row3, row4)
    -- * Converting 'Braille6' to 'Braille'
  , toBraille', toBraille
    -- * Rendering Braille characters.
  , braille6, braille
  ) where

import Data.Bits((.|.), shiftL)
import Data.Bool(bool)
import Data.Char(chr)
import Data.Char.Block(Row(Row))

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

-- | A datastructure to render Braille patterns with six dots cells.
data Braille6 a = Braille6 {
    top :: Row a -- ^ The state of the top row of the Braille character.
  , middle :: Row a -- ^ The state of the middle row of the Braille character.
  , bottom :: Row a -- ^ The state of the bottom row of the Braille character.
  } deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

-- | A datastructure to render Braille patterns with eight dots cells.
data Braille a = Braille {
    row1 :: Row a -- ^ The state of the top row of the Braille character.
  , row2 :: Row a -- ^ The state of the second row of the Braille character.
  , row3 :: Row a -- ^ The state of the third row of the Braille character.
  , row4 :: Row a -- ^ The state of the bottom row of the Braille character.
  } deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

-- | Convert a 'Braille6' value to a 'Braille' character, by putting in a given
-- value at the two values at the bottom row.
toBraille'
  :: a -- ^ The value to put in the cells of the bottom row.
  -> Braille6 a -- ^ The given 'Braille6' value to convert.
  -> Braille a -- ^ A 'Braille' value that uses as bottom two values given as first parameter.
toBraille' d (Braille6 r0 r1 r2) = Braille r0 r1 r2 (Row d d)

-- | Convert a 'Braille6' value to a 'Braille6' character by setting the bottom
-- row with two 'False' values.
toBraille
  :: Braille6 Bool -- ^ The given 'Braille6' value to convert.
  -> Braille Bool -- ^ A 'Braille' value that uses as bottom two times 'False'.
toBraille = toBraille' False

instance Arbitrary a => Arbitrary (Braille6 a) where
    arbitrary = arbitrary1

instance Arbitrary1 Braille6 where
    liftArbitrary arb = Braille6 <$> arb' <*> arb' <*> arb'
        where arb' = liftArbitrary arb

instance Arbitrary a => Arbitrary (Braille a) where
    arbitrary = arbitrary1

instance Arbitrary1 Braille where
    liftArbitrary arb = Braille <$> arb' <*> arb' <*> arb' <*> arb'
        where arb' = liftArbitrary arb

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

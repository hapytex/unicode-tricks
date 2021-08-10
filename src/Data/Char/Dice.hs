{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, Safe, TypeApplications #-}

{-|
Module      : Data.Char.Dice
Description : Support for dice characters in unicode.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has a <https://www.unicode.org/charts/PDF/U2600.pdf block> named /Miscellaneous Symbols/ that includes unicode characters for dice, this module aims to make it more convenient to render
die characters.
-}

module Data.Char.Dice(
    -- * Represent die values
    DieValue(I, II, III, IV, V, VI)
    -- * Convert to a die value
  , toDieValue
    -- * Render a die
  , die
  ) where

import Control.DeepSeq(NFData)

import Data.Bits((.|.))
import Data.Char(chr)
import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar', isInCharRange), UnicodeText(isInTextRange), generateIsInTextRange', mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_dieOffset :: Int
_dieOffset = 0x2680

-- | A data type to store the values of a die.
data DieValue
  = I  -- ^ A die with value /one/, represented with ⚀.
  | II  -- ^ A die with value /two/, represented with ⚁.
  | III  -- ^ A die with value /three/, represented with ⚂.
  | IV  -- ^ A die with value /four/, represented with ⚃.
  | V  -- ^ A die with value /five/, represented with ⚄.
  | VI  -- ^ A die with value /six/, represented with ⚅.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable DieValue

instance NFData DieValue

-- | Convert the given integral value to a 'DieValue' that represents the given
-- number. If the number is less than one, or greater than six, 'Nothing' is
-- returned.
toDieValue :: Integral i
  => i  -- ^ The given integral value to convert to a 'DieValue'.
  -> Maybe DieValue  -- ^ A 'DieValue' wrapped in a 'Just' if the given integral value is greater than zero, and less than seven, otherwise 'Nothing'.
toDieValue i
    | i > 0 && i <= 6 = Just (toEnum (fromIntegral i-1))
    | otherwise = Nothing

instance Arbitrary DieValue where
    arbitrary = arbitraryBoundedEnum

-- | Convert the given 'DieValue' to a unicode character that represents a /die/
-- with that value.
die
  :: DieValue  -- ^ The die value to convert.
  -> Char  -- ^ A unicode character that represents a die with the given 'DieValue'.
die = chr . (_dieOffset .|.) . fromEnum

instance UnicodeCharacter DieValue where
    toUnicodeChar = mapFromEnum _dieOffset
    fromUnicodeChar = mapToEnumSafe _dieOffset
    fromUnicodeChar' = mapToEnum _dieOffset
    isInCharRange c = '\x2680' <= c && c <= '\x2685'

instance UnicodeText DieValue where
  isInTextRange = generateIsInTextRange' @DieValue

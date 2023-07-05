{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.Char.Number.Mayan
-- Description : A module to print Mayan numerals.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Unicode uses a <https://www.unicode.org/charts/PDF/U1D2E0.pdf code block> for Mayan numerals. Mayan numerals are written top to bottom,
-- so vertically. This module aims to make it more convenient to write Mayan numerals by offering
-- functions to convert numbers into a 'Text' object for Mayan numbers. Mayan numerals can /not/
-- present /negative/ numbers.
module Data.Char.Number.Mayan
  ( -- * Define Mayan literals
    MayanLiteral (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Eleven, Twelve, Thirteen, Fourteen, Fifteen, Sixteen, Seventeen, Eighteen, Nineteen),

    -- * Converting integers to Mayan numbers.
    toMayanVertical,
    toMayanVertical',
    toMayanVertical'',
    toMayan,
    toMayan',
    toMayan'',
  )
where

import Control.DeepSeq (NFData)
import Data.Char.Core (UnicodeCharacter (fromUnicodeChar, fromUnicodeChar', isInCharRange, toUnicodeChar), UnicodeText (isInTextRange), generateIsInTextRange', mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)

-- | The Mayan numerals, as defined in the Unicode block.
data MayanLiteral
  = -- | The unicode character for the Mayan numeral /zero/: 𝋠.
    Zero
  | -- | The unicode character for the Mayan numeral /one/: 𝋡.
    One
  | -- | The unicode character for the Mayan numeral /two/: 𝋢.
    Two
  | -- | The unicode character for the Mayan numeral /three/: 𝋣.
    Three
  | -- | The unicode character for the Mayan numeral /four/: 𝋤.
    Four
  | -- | The unicode character for the Mayan numeral /five/: 𝋥.
    Five
  | -- | The unicode character for the Mayan numeral /six/: 𝋦.
    Six
  | -- | The unicode character for the Mayan numeral /seven/: 𝋧.
    Seven
  | -- | The unicode character for the Mayan numeral /eight/: 𝋨.
    Eight
  | -- | The unicode character for the Mayan numeral /nine/: 𝋩.
    Nine
  | -- | The unicode character for the Mayan numeral /ten/: 𝋪.
    Ten
  | -- | The unicode character for the Mayan numeral /eleven/: 𝋫.
    Eleven
  | -- | The unicode character for the Mayan numeral /twelve/: 𝋬.
    Twelve
  | -- | The unicode character for the Mayan numeral /thirteen/: 𝋭.
    Thirteen
  | -- | The unicode character for the Mayan numeral /fourteen/: 𝋮.
    Fourteen
  | -- | The unicode character for the Mayan numeral /fifteen/: 𝋯.
    Fifteen
  | -- | The unicode character for the Mayan numeral /sixteen/: 𝋰.
    Sixteen
  | -- | The unicode character for the Mayan numeral /seventeen/: 𝋱.
    Seventeen
  | -- | The unicode character for the Mayan numeral /eighteen/: 𝋲.
    Eighteen
  | -- | The unicode character for the Mayan numeral /nineteen/: 𝋳.
    Nineteen
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance NFData MayanLiteral

_mayanOffset :: Int
_mayanOffset = 0x1d2e0

instance Arbitrary MayanLiteral where
  arbitrary = arbitraryBoundedEnum

instance Hashable MayanLiteral

instance UnicodeCharacter MayanLiteral where
  toUnicodeChar = mapFromEnum _mayanOffset
  fromUnicodeChar = mapToEnumSafe _mayanOffset
  fromUnicodeChar' = mapToEnum _mayanOffset
  isInCharRange c = '\x1d2e0' <= c && c <= '\x1d2f3'

instance UnicodeText MayanLiteral where
  isInTextRange = generateIsInTextRange' @MayanLiteral

-- | Convert the given 'Integral' number to a 'Text' object that writes the Mayan number to to bottom.
-- This function will return a 'Nothing' in case the number is negative (since it can not be presented
-- in Mayan).
toMayanVertical ::
  Integral i =>
  -- | The given number to convert to a /vertical/ 'String' object.
  i ->
  -- | A 'Text' that contains the Mayan number wrapped in a 'Just' if we can represent the number; 'Nothing' otherwise.
  Maybe Text
toMayanVertical n
  | n < 0 = Nothing
  | otherwise = Just (toMayanVertical' n)

-- | Convert the given 'Integral' number to a 'Text' object that writes the Mayan number to to bottom.
toMayanVertical' ::
  Integral i =>
  -- | The given number to convert to a /vertical/ 'String' object.
  i ->
  -- | A 'Text' that contains the Mayan number.
  Text
toMayanVertical' = pack . toMayanVertical''

-- | Convert the given 'Integral' number to a 'String' object that writes the Mayan number to to bottom.
toMayanVertical'' ::
  Integral i =>
  -- | The given number to convert to a /vertical/ 'String' object.
  i ->
  -- | A 'String' that contains the Mayan number.
  String
toMayanVertical'' = unlines . map pure . toMayan''

-- | Convert the given 'Integral' number to a 'Text' object that writes the Mayan number from left-to-right.
-- The object is wrapped in a 'Just' data constructor. If the number is negative, and thus can not be
-- represented, 'Nothing' is returned.
toMayan ::
  Integral i =>
  -- | The given number to convert to a /horizontal/ 'String' object.
  i ->
  -- | A 'Text' that contains the Mayan number wrapped in a 'Just' if we can represent the number; 'Nothing' otherwise.
  Maybe Text
toMayan n
  | n < 0 = Nothing
  | otherwise = Just (toMayan' n)

-- | Convert the given 'Integral' number to a 'Text' object that writes the Mayan number from left-to-right.
toMayan' ::
  Integral i =>
  -- | The given number to convert to a /horizontal/ 'String' object.
  i ->
  -- | A 'Text' that contains the Mayan number.
  Text
toMayan' = pack . toMayan''

-- | Convert the given 'Integral' number to a 'String' object that writes the Mayan number from left-to-right.
toMayan'' ::
  Integral i =>
  -- | The given number to convert to a /horizontal/ 'String' object.
  i ->
  -- | A 'String' that contains the Mayan number.
  String
toMayan'' = go []
  where
    go xs n
      | n <= 19 = ch n : xs
      | otherwise = go (ch r : xs) q
      where
        ~(q, r) = n `quotRem` 20
        ch = toUnicodeChar . toEnum @MayanLiteral . fromIntegral

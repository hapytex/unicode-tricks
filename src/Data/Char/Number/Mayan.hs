{-# LANGUAGE Safe, TypeApplications #-}

{-|
Module      : Data.Char.Number.Mayan
Description : A module to print Mayan numerals.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode uses a <https://www.unicode.org/charts/PDF/U1D2E0.pdf code block> for Mayan numerals. Mayan numerals are written top to bottom,
so vertically. This module aims to make it more convenient to write Mayan numerals by offering
functions to convert numbers into a 'Text' object for Mayan numbers.
-}

module Data.Char.Number.Mayan (
    -- * Define Mayan literals
    MayanLiteral(Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Eleven, Twelve, Thirteen, Fourteen, Fifteen, Sixteen, Seventeen, Eighteen, Nineteen)
    -- * Converting integers to Mayan numbers.
  , toMayanVertical, toMayanVertical', toMayan, toMayan'
  ) where

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar'), UnicodeText, mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Text(Text, pack)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- | The Mayan numerals, as defined in the Unicode block.
data MayanLiteral
  = Zero  -- ^ The unicode character for the Mayan numeral /zero/: 𝋠.
  | One  -- ^ The unicode character for the Mayan numeral /one/: 𝋡.
  | Two  -- ^ The unicode character for the Mayan numeral /two/: 𝋢.
  | Three  -- ^ The unicode character for the Mayan numeral /three/: 𝋣.
  | Four  -- ^ The unicode character for the Mayan numeral /four/: 𝋤.
  | Five  -- ^ The unicode character for the Mayan numeral /five/: 𝋥.
  | Six  -- ^ The unicode character for the Mayan numeral /six/: 𝋦.
  | Seven  -- ^ The unicode character for the Mayan numeral /seven/: 𝋧.
  | Eight  -- ^ The unicode character for the Mayan numeral /eight/: 𝋨.
  | Nine  -- ^ The unicode character for the Mayan numeral /nine/: 𝋩.
  | Ten  -- ^ The unicode character for the Mayan numeral /ten/: 𝋪.
  | Eleven  -- ^ The unicode character for the Mayan numeral /eleven/: 𝋫.
  | Twelve  -- ^ The unicode character for the Mayan numeral /twelve/: 𝋬.
  | Thirteen  -- ^ The unicode character for the Mayan numeral /thirteen/: 𝋭.
  | Fourteen  -- ^ The unicode character for the Mayan numeral /fourteen/: 𝋮.
  | Fifteen  -- ^ The unicode character for the Mayan numeral /fifteen/: 𝋯.
  | Sixteen  -- ^ The unicode character for the Mayan numeral /sixteen/: 𝋰.
  | Seventeen  -- ^ The unicode character for the Mayan numeral /seventeen/: 𝋱.
  | Eighteen  -- ^ The unicode character for the Mayan numeral /eighteen/: 𝋲.
  | Nineteen  -- ^ The unicode character for the Mayan numeral /nineteen/: 𝋳.
 deriving (Bounded, Enum, Eq, Ord, Read, Show)

_mayanOffset :: Int
_mayanOffset = 0x1d2e0

instance Arbitrary MayanLiteral where
  arbitrary = arbitraryBoundedEnum

instance UnicodeCharacter MayanLiteral where
    toUnicodeChar = mapFromEnum _mayanOffset
    fromUnicodeChar = mapToEnumSafe _mayanOffset
    fromUnicodeChar' = mapToEnum _mayanOffset

instance UnicodeText MayanLiteral

-- | Convert the given 'Integral' number to a 'Text' object that writes the Mayan number to to bottom.
toMayanVertical :: Integral i
  => i -- ^ The given number to convert to a /vertical/ 'String' object.
  -> Text  -- ^ A 'Text' that contains the Mayan number.
toMayanVertical = pack . toMayanVertical'

-- | Convert the given 'Integral' number to a 'String' object that writes the Mayan number to to bottom.
toMayanVertical' :: Integral i
  => i -- ^ The given number to convert to a /vertical/ 'String' object.
  -> String  -- ^ A 'String' that contains the Mayan number.
toMayanVertical' = unlines . map pure . toMayan'

-- | Convert the given 'Integral' number to a 'Text' object that writes the Mayan number from left-to-right.
toMayan :: Integral i
  => i -- ^ The given number to convert to a /horizontal/ 'String' object.
  -> Text  -- ^ A 'Text' that contains the Mayan number.
toMayan = pack . toMayan'

-- | Convert the given 'Integral' number to a 'String' object that writes the Mayan number from left-to-right.
toMayan' :: Integral i
  => i -- ^ The given number to convert to a /horizontal/ 'String' object.
  -> String  -- ^ A 'String' that contains the Mayan number.
toMayan' = go []
  where go xs n | n <= 19 = ch n: xs
                | otherwise = go (ch r:xs) q
          where ~(q, r) = n `quotRem` 20
                ch = toUnicodeChar . toEnum @MayanLiteral . fromIntegral

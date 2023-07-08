{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.Char.Number.Tally
-- Description : A module to print Tally numerals.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module aims to convert numbers to (Western) tally marks and vice versa.
module Data.Char.Number.Tally
  ( -- * Data types to represent tally marks
    TallyLiteral (I, V),

    -- * Convert a number to 'TallyLiteral's
    toLiterals,
    toLiterals',
    tallyNumber,
    tallyNumber',
  )
where

import Control.DeepSeq (NFData)
import Data.Char.Core (UnicodeCharacter (fromUnicodeChar, fromUnicodeChar', isInCharRange, toUnicodeChar), UnicodeText (isInTextRange), generateIsInTextRange', mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.List (genericReplicate)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)

_tallyOffset :: Int
_tallyOffset = 0x1d377

-- | A tally literal that is either a one (𝍷), or five grouped together (𝍸).
data TallyLiteral
  = -- | The unicode character for the tally numeral /one/: 𝍷.
    I
  | -- | The unicode character for the tally numeral /five/: 𝍸.
    V
  deriving (Bounded, Data, Enum, Eq, Generic, Show, Read)

instance Arbitrary TallyLiteral where
  arbitrary = arbitraryBoundedEnum

instance UnicodeCharacter TallyLiteral where
  toUnicodeChar = mapFromEnum _tallyOffset
  fromUnicodeChar = mapToEnumSafe _tallyOffset
  fromUnicodeChar' = mapToEnum _tallyOffset
  isInCharRange c = '\x1d377' <= c && c <= '\x1d378'

instance UnicodeText TallyLiteral where
  isInTextRange = generateIsInTextRange' @TallyLiteral

instance Hashable TallyLiteral

instance NFData TallyLiteral

-- | Convert a given /positive/ natural number to a sequence of 'TallyLiteral's.
toLiterals ::
  Integral i =>
  -- | The given number to convert.
  i ->
  -- | A list of 'TallyLiteral's if the given number can be specified with tally marks, 'Nothing' otherwise.
  Maybe [TallyLiteral]
toLiterals k
  | k > 0 = Just (toLiterals' k)
  | otherwise = Nothing

-- | Convert a given number to a sequence of 'TallyLiteral's, for negative numbers or zero, the behavior is unspecified.
toLiterals' ::
  Integral i =>
  -- | The given number to convert.
  i ->
  -- | A list of 'TallyLiteral's that denotes the given number.
  [TallyLiteral]
toLiterals' k = genericReplicate k0 V ++ genericReplicate k1 I
  where
    ~(k0, k1) = k `divMod` 5

-- | Convert a given /positive/ natural number to a 'Text' object with the tally marks for that number.
tallyNumber ::
  Integral i =>
  -- | The given number to convert.
  i ->
  -- | A 'Text' with the tally marks wrapped in a 'Just' if the number can be represented with tally marks; 'Nothing' otherwise.
  Maybe Text
tallyNumber k
  | k > 0 = Just (tallyNumber' k)
  | otherwise = Nothing

-- | Convert a given number to a 'Text' object with the tally marks for that number, for negative numbers or zero, the behavior is unspecified.
tallyNumber' ::
  Integral i =>
  -- | The given number to convert.
  i ->
  -- | The corresponding 'Text' that contains the number as /tally marks/.
  Text
tallyNumber' = pack . map toUnicodeChar . toLiterals'

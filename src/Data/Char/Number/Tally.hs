{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, Safe, TypeApplications #-}

{-|
Module      : Data.Char.Number.Tally
Description : A module to print Tally numerals.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module aims to convert numbers to (Western) tally marks and vice versa.
-}

module Data.Char.Number.Tally (
    -- * Data types to represent tally marks
    TallyLiteral(I, V)
  ) where

import Control.DeepSeq(NFData)

import Data.Bits((.|.))
import Data.Char(chr)
import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar', isInCharRange), UnicodeText(isInTextRange), generateIsInTextRange', LetterCase, Ligate, ligateF, mapFromEnum, mapToEnum, mapToEnumSafe, splitLetterCase)
import Data.Data(Data)
import Data.Default.Class(Default(def))
import Data.Hashable(Hashable)
import Data.Text(Text, cons, empty)
import Data.List(genericReplicate)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_tallyOffset :: Int
_tallyOffset = 0x1d377

-- | A tally literal that is either a one (𝍷), or five grouped together (𝍸).
data TallyLiteral
  = I  -- ^ The unicode character for the tally numeral /one/: 𝍷.
  | V  -- ^ The unicode character for the tally numeral /five/: 𝍸.
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


-- | Convert a given /positive/ natural number to a sequence of
toLiterals :: Integral i
  => i  -- ^ The given number to convert.
  -> Maybe [TallyLiteral]  -- ^ A list of 'TallyLiteral's if the given number can be specified with tally marks, 'Nothing' otherwise.
toLiterals k
    | k > 0 = Just (genericReplicate k0 V ++ genericReplicate k1 I)
    | otherwise = Nothing
    where ~(k0, k1) = k `divMod` 5

-- | Convert a given number to a 'Text' wrapped in a 'Just' data constructor,
-- given the number, given it can be represented. 'Nothing' in case it can not
-- be represented. The number is written in Roman numerals in /upper case/.
-- tallyMarks :: Integral i
--   -> i  -- ^ The given number to convert.
--   -> Maybe Text  -- ^ A 'Text' if the given number can be specified with Roman
                -- numerals wrapped in a 'Just', 'Nothing' otherwise.

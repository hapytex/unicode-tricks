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
  isInCharRange c = '\x2160' <= c && c <= '\x216f'

instance UnicodeText TallyLiteral where
  isInTextRange = generateIsInTextRange' @TallyLiteral

instance Hashable TallyLiteral

instance NFData TallyLiteral


toLiterals :: Integral i
  -> i  -- ^ The given number to convert.
  -> Maybe [TallyLiteral]  -- ^ A list of 'TallyLiteral's if the given number can be specified
                          -- with Roman numerals, 'Nothing' otherwise.
toLiterals k
    | k > 0 = Just (genericReplicate k0 V ++ genericReplicate k1 I)
    | otherwise = Nothing
    where ~(k0, k1) = k `divMod` 5

-- | Convert the given 'TallyLiteral' object to a unicode character in
-- /upper case/.
romanLiteral
  :: TallyLiteral  -- ^ The given 'TallyLiteral' to convert.
  -> Char  -- ^ A unicode character that represents the given 'TallyLiteral'.
romanLiteral = _romanLiteral _tallyOffset

-- | Convert the given 'TallyLiteral' object to a unicode character in
-- /lower case/.
romanLiteral'
  :: TallyLiteral  -- ^ The given 'TallyLiteral' to convert.
  -> Char  -- ^ A unicode character that represents the given 'TallyLiteral'.
romanLiteral' = _romanLiteral _romanLowercaseOffset

_romanNumeral :: (TallyLiteral -> Char) -> [TallyLiteral] -> Text
_romanNumeral = (`foldr` empty) . (cons .)

-- | Convert a sequence of 'TallyLiteral' objects to a 'Text' object that
-- contains a sequence of corresponding Unicode characters which are Roman
-- numberals in /upper case/.
romanNumeral
  :: [TallyLiteral]  -- ^ The given list of 'TallyLiteral' objects to convert to a Unicode equivalent.
  -> Text  -- ^ A 'Text' object that contains a sequence of unicode characters that represents the 'TallyLiteral's.
romanNumeral = _romanNumeral romanLiteral


-- | Convert a sequence of 'TallyLiteral' objects to a 'Text' object that
-- contains a sequence of corresponding Unicode characters which are Roman
-- numberals in /lower case/.
romanNumeral'
  :: [TallyLiteral]  -- ^ The given list of 'TallyLiteral' objects to convert to a Unicode equivalent.
  -> Text  -- ^ A 'Text' object that contains a sequence of unicode characters that represents the 'TallyLiteral's.
romanNumeral' = _romanNumeral romanLiteral'

-- | Convert a sequence of 'TallyLiteral' objects to a 'Text' object that
-- contains a sequence of corresponding Unicode characters which are Roman
-- numberals in /upper case/ or /lower case/ depending on the 'LetterCase' value.
romanNumeralCase
  :: LetterCase  -- ^ The given 'LetterCase' to apply.
  -> [TallyLiteral]  -- ^ The given list of 'TallyLiteral' objects to convert to a Unicode equivalent.
  -> Text  -- ^ A 'Text' object that contains a sequence of unicode characters that represents the 'TallyLiteral's.
romanNumeralCase = splitLetterCase romanNumeral romanNumeral'

_romanNumber :: Integral i => ([TallyLiteral] -> a) -> RomanStyle -> Ligate -> i -> Maybe a
_romanNumber f r c = fmap f . toLiterals r c

-- | Convert a given number to a 'Text' wrapped in a 'Just' data constructor,
-- given the number, given it can be represented. 'Nothing' in case it can not
-- be represented. The number is written in Roman numerals in /upper case/.
romanNumber :: Integral i
  => RomanStyle  -- ^ Specifies if the Numeral is 'Additive' or 'Subtractive' style.
  -> Ligate  -- ^ Specifies if characters like @ⅠⅤ@ are joined to @Ⅳ@.
  -> i  -- ^ The given number to convert.
  -> Maybe Text  -- ^ A 'Text' if the given number can be specified with Roman
                -- numerals wrapped in a 'Just', 'Nothing' otherwise.
romanNumber = _romanNumber romanNumeral

-- | Convert a given number to a 'Text' wrapped in a 'Just' data constructor,
-- given the number, given it can be represented. 'Nothing' in case it can not
-- be represented. The number is written in Roman numerals in /lower case/.
romanNumber' :: Integral i
  => RomanStyle  -- ^ Specifies if the Numeral is 'Additive' or 'Subtractive' style.
  -> Ligate  -- ^ Specifies if characters like @ⅠⅤ@ are joined to @Ⅳ@.
  -> i  -- ^ The given number to convert.
  -> Maybe Text  -- ^ A 'Text' if the given number can be specified with Roman
                -- numerals wrapped in a 'Just', 'Nothing' otherwise.
romanNumber' = _romanNumber romanNumeral'

-- | Convert a given number to a 'Text' wrapped in a 'Just' data constructor,
-- given the number, given it can be represented. 'Nothing' in case it can not
-- be represented. The number is written in Roman numerals in /upper case/ or
-- /lower case/ depending on the 'LetterCase' value.
romanNumberCase :: Integral i
  => LetterCase
  -> RomanStyle
  -> Ligate
  -> i
  -> Maybe Text
romanNumberCase = splitLetterCase romanNumber romanNumber'

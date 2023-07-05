{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.Char.Number.Roman
-- Description : A module to print Roman numerals both in upper case and lower case.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module aims to convert Roman numerals to a String of unicode characters that
-- represent the corresponding Roman number.
--
-- One can convert numbers to Roman numerals in upper case and lower case, and in 'Additive' and 'Subtractive' style.
module Data.Char.Number.Roman
  ( -- * Data types to represent Roman numerals
    RomanLiteral (I, II, III, IV, V, VI, VII, VIII, IX, X, XI, XII, L, C, D, M),
    RomanStyle (Additive, Subtractive),

    -- * Convert a number to Roman literals
    toLiterals,
    romanLiteral,
    romanLiteral',

    -- * Convert a number to text
    romanNumeral,
    romanNumeral',
    romanNumeralCase,
    romanNumber,
    romanNumber',
    romanNumberCase,
  )
where

import Control.DeepSeq (NFData)
import Data.Bits ((.|.))
import Data.Char (chr)
import Data.Char.Core (LetterCase, Ligate, UnicodeCharacter (fromUnicodeChar, fromUnicodeChar', isInCharRange, toUnicodeChar), UnicodeText (isInTextRange), generateIsInTextRange', ligateF, mapFromEnum, mapToEnum, mapToEnumSafe, splitLetterCase)
import Data.Data (Data)
import Data.Default.Class (Default (def))
import Data.Hashable (Hashable)
import Data.Text (Text, cons, empty)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)

-- | The style to convert a number to a Roman numeral. The 'UnicodeCharacter'
-- instance maps on the uppercase Roman literals.
data RomanStyle
  = -- | The additive style converts four to ⅠⅠⅠⅠ.
    Additive
  | -- | The subtractive style converts four to ⅠⅤ.
    Subtractive
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary RomanStyle where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RomanLiteral where
  arbitrary = arbitraryBoundedEnum

instance Default RomanStyle where
  def = Subtractive

instance Hashable RomanStyle

instance NFData RomanStyle

instance UnicodeCharacter RomanLiteral where
  toUnicodeChar = mapFromEnum _romanUppercaseOffset
  fromUnicodeChar = mapToEnumSafe _romanUppercaseOffset
  fromUnicodeChar' = mapToEnum _romanUppercaseOffset
  isInCharRange c = '\x2160' <= c && c <= '\x216f'

instance UnicodeText RomanLiteral where
  isInTextRange = generateIsInTextRange' @RomanLiteral

-- | Roman numerals for which a unicode character exists.
data RomanLiteral
  = -- | The unicode character for the Roman numeral /one/: Ⅰ.
    I
  | -- | The unicode character for the Roman numeral /two/: Ⅱ.
    II
  | -- | The unicode character for the Roman numeral /three/: Ⅲ.
    III
  | -- | The unicode character for the Roman numeral /four/: Ⅳ.
    IV
  | -- | The unicode character for the Roman numeral /five/: Ⅴ.
    V
  | -- | The unicode character for the Roman numeral /six/: Ⅵ.
    VI
  | -- | The unicode character for the Roman numeral /seven/: Ⅶ.
    VII
  | -- | The unicode character for the Roman numeral /eight/: Ⅷ.
    VIII
  | -- | The unicode character for the Roman numeral /nine/: Ⅸ.
    IX
  | -- | The unicode character for the Roman numeral /ten/: Ⅹ.
    X
  | -- | The unicode character for the Roman numeral /eleven/: Ⅺ.
    XI
  | -- | The unicode character for the Roman numeral /twelve/: Ⅻ.
    XII
  | -- | The unicode character for the Roman numeral /fifty/: Ⅼ.
    L
  | -- | The unicode character for the Roman numeral /hundred/: Ⅽ.
    C
  | -- | The unicode character for the Roman numeral /five hundred/: Ⅾ.
    D
  | -- | The unicode character for the Roman numeral /thousand/: Ⅿ.
    M
  deriving (Bounded, Data, Enum, Eq, Generic, Show, Read)

instance Hashable RomanLiteral

instance NFData RomanLiteral

_literals :: Integral i => RomanStyle -> [(i, [RomanLiteral] -> [RomanLiteral])]
_literals Additive =
  [ (1000, (M :)),
    (500, (D :)),
    (100, (C :)),
    (50, (L :)),
    (10, (X :)),
    (5, (V :)),
    (1, (I :))
  ]
_literals Subtractive =
  [ (1000, (M :)),
    (900, ([C, M] ++)),
    (500, (D :)),
    (400, ([C, D] ++)),
    (100, (C :)),
    (90, ([X, C] ++)),
    (50, (L :)),
    (40, ([X, L] ++)),
    (10, (X :)),
    (9, ([I, X] ++)),
    (5, (V :)),
    (4, ([I, V] ++)),
    (1, (I :))
  ]

_ligate :: [RomanLiteral] -> [RomanLiteral]
_ligate [] = []
_ligate (r : rs) = go r rs
  where
    go x [] = [x]
    go x (y : ys) = f x y ys
    f I I = go II
    f II I = skip III
    f I V = skip IV
    f V I = go VI
    f VI I = go VII
    f VII I = skip VIII
    f X I = go XI
    f I X = skip IX
    f XI I = go XII
    f x y = (x :) . go y
    skip = (. _ligate) . (:)

-- | Convert the given number with the given 'RomanStyle' and 'Ligate' style
-- to a sequence of 'RomanLiteral's, given the number can be represented
-- with Roman numerals (is strictly larger than zero).
toLiterals ::
  Integral i =>
  -- | Specifies if the Numeral is 'Additive' or 'Subtractive' style.
  RomanStyle ->
  -- | Specifies if characters like @ⅠⅤ@ are joined to @Ⅳ@.
  Ligate ->
  -- | The given number to convert.
  i ->
  -- | A list of 'RomanLiteral's if the given number can be specified
  -- with Roman numerals, 'Nothing' otherwise.
  Maybe [RomanLiteral]
toLiterals s c k
  | k > 0 = ligateF _ligate c (go k (_literals s))
  | otherwise = Nothing
  where
    go 0 _ = Just []
    go _ [] = Nothing
    go n va@((m, l) : vs)
      | n >= m = l <$> go (n - m) va
      | otherwise = go n vs

_romanUppercaseOffset :: Int
_romanUppercaseOffset = 0x2160

_romanLowercaseOffset :: Int
_romanLowercaseOffset = 0x2170

_romanLiteral :: Int -> RomanLiteral -> Char
_romanLiteral = (chr .) . (. fromEnum) . (.|.)

-- | Convert the given 'RomanLiteral' object to a unicode character in
-- /upper case/.
romanLiteral ::
  -- | The given 'RomanLiteral' to convert.
  RomanLiteral ->
  -- | A unicode character that represents the given 'RomanLiteral'.
  Char
romanLiteral = _romanLiteral _romanUppercaseOffset

-- | Convert the given 'RomanLiteral' object to a unicode character in
-- /lower case/.
romanLiteral' ::
  -- | The given 'RomanLiteral' to convert.
  RomanLiteral ->
  -- | A unicode character that represents the given 'RomanLiteral'.
  Char
romanLiteral' = _romanLiteral _romanLowercaseOffset

_romanNumeral :: (RomanLiteral -> Char) -> [RomanLiteral] -> Text
_romanNumeral = (`foldr` empty) . (cons .)

-- | Convert a sequence of 'RomanLiteral' objects to a 'Text' object that
-- contains a sequence of corresponding Unicode characters which are Roman
-- numberals in /upper case/.
romanNumeral ::
  -- | The given list of 'RomanLiteral' objects to convert to a Unicode equivalent.
  [RomanLiteral] ->
  -- | A 'Text' object that contains a sequence of unicode characters that represents the 'RomanLiteral's.
  Text
romanNumeral = _romanNumeral romanLiteral

-- | Convert a sequence of 'RomanLiteral' objects to a 'Text' object that
-- contains a sequence of corresponding Unicode characters which are Roman
-- numberals in /lower case/.
romanNumeral' ::
  -- | The given list of 'RomanLiteral' objects to convert to a Unicode equivalent.
  [RomanLiteral] ->
  -- | A 'Text' object that contains a sequence of unicode characters that represents the 'RomanLiteral's.
  Text
romanNumeral' = _romanNumeral romanLiteral'

-- | Convert a sequence of 'RomanLiteral' objects to a 'Text' object that
-- contains a sequence of corresponding Unicode characters which are Roman
-- numberals in /upper case/ or /lower case/ depending on the 'LetterCase' value.
romanNumeralCase ::
  -- | The given 'LetterCase' to apply.
  LetterCase ->
  -- | The given list of 'RomanLiteral' objects to convert to a Unicode equivalent.
  [RomanLiteral] ->
  -- | A 'Text' object that contains a sequence of unicode characters that represents the 'RomanLiteral's.
  Text
romanNumeralCase = splitLetterCase romanNumeral romanNumeral'

_romanNumber :: Integral i => ([RomanLiteral] -> a) -> RomanStyle -> Ligate -> i -> Maybe a
_romanNumber f r c = fmap f . toLiterals r c

-- | Convert a given number to a 'Text' wrapped in a 'Just' data constructor,
-- given the number, given it can be represented. 'Nothing' in case it can not
-- be represented. The number is written in Roman numerals in /upper case/.
romanNumber ::
  Integral i =>
  -- | Specifies if the Numeral is 'Additive' or 'Subtractive' style.
  RomanStyle ->
  -- | Specifies if characters like @ⅠⅤ@ are joined to @Ⅳ@.
  Ligate ->
  -- | The given number to convert.
  i ->
  -- | A 'Text' if the given number can be specified with Roman
  -- numerals wrapped in a 'Just', 'Nothing' otherwise.
  Maybe Text
romanNumber = _romanNumber romanNumeral

-- | Convert a given number to a 'Text' wrapped in a 'Just' data constructor,
-- given the number, given it can be represented. 'Nothing' in case it can not
-- be represented. The number is written in Roman numerals in /lower case/.
romanNumber' ::
  Integral i =>
  -- | Specifies if the Numeral is 'Additive' or 'Subtractive' style.
  RomanStyle ->
  -- | Specifies if characters like @ⅠⅤ@ are joined to @Ⅳ@.
  Ligate ->
  -- | The given number to convert.
  i ->
  -- | A 'Text' if the given number can be specified with Roman
  -- numerals wrapped in a 'Just', 'Nothing' otherwise.
  Maybe Text
romanNumber' = _romanNumber romanNumeral'

-- | Convert a given number to a 'Text' wrapped in a 'Just' data constructor,
-- given the number, given it can be represented. 'Nothing' in case it can not
-- be represented. The number is written in Roman numerals in /upper case/ or
-- /lower case/ depending on the 'LetterCase' value.
romanNumberCase ::
  Integral i =>
  LetterCase ->
  RomanStyle ->
  Ligate ->
  i ->
  Maybe Text
romanNumberCase = splitLetterCase romanNumber romanNumber'

{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math.SansSerif.Digit
-- Description : Sans serif mathematical alphanumeric symbols
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- See "Data.Char.Math" for further documentation.
module Data.Char.Math.SansSerif.Digit
  ( -- * Character conversion
    digitSansSerif,
    digitSansSerif',
    digitSansSerifRegular,
    digitSansSerifRegular',
    digitSansSerifBold,
    digitSansSerifBold',

    -- * Int to digit characters
    intToDigitSansSerif,
    intToDigitSansSerif',
    intToDigitSansSerifRegular,
    intToDigitSansSerifRegular',
    intToDigitSansSerifBold,
    intToDigitSansSerifBold',
  )
where

import Data.Char (intToDigit, isDigit)
import Data.Char.Core (Emphasis, splitEmphasis)
import Data.Char.Math.Internal

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- in a non-bold sans-serif style. The result for characters outside this range is
-- unspecified.
digitSansSerifRegular' ::
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in sans-serifs not in bold, unspecified outside the the range.
  Char
digitSansSerifRegular' = _shiftC 0x1d7b2

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- in a non-bold sans-serif style wrapped in a 'Just' data constructor. For
-- characters outside this range, 'Nothing' is returned.
digitSansSerifRegular ::
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in sans-serifs not in bold wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
digitSansSerifRegular = _withCondition isDigit digitSansSerifRegular'

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- in a bold sans-serif style. The result for characters outside this range is
-- unspecified.
digitSansSerifBold' ::
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in sans-serifs in bold, unspecified outside the the range.
  Char
digitSansSerifBold' = _shiftC 0x1d7bc

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- in a bold sans-serif style wrapped in a 'Just' data constructor. For
-- characters outside this range, 'Nothing' is returned.
digitSansSerifBold ::
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in sans-serifs in bold wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
digitSansSerifBold = _withCondition isDigit digitSansSerifBold'

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- with a given 'Emphasis' in sans-serif style. The result for characters outside this
-- range is unspecified.
digitSansSerif' ::
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in sans-serifs for the given /emphasis/ style, unspecified outside the the range.
  Char
digitSansSerif' = splitEmphasis digitSansSerifRegular' digitSansSerifBold'

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- with the given 'Emphasis' in sans-serif style wrapped in a 'Just' data constructor.
-- For characters outside this range, 'Nothing' is returned.
digitSansSerif ::
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in sans-serifs for the given /emphasis/ style wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
digitSansSerif = splitEmphasis digitSansSerifRegular digitSansSerifBold

-- | Convert the given number (@0@–@9@) to its corresponding character
-- in a non-bold sans-serif style. The result for numbers outside this range is
-- unspecified.
intToDigitSansSerifRegular' ::
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in sans-serifs not in bold, unspecified outside the the range.
  Char
intToDigitSansSerifRegular' = digitSansSerifRegular' . intToDigit

-- | Convert the given number (@0@–@9@) to its corresponding character
-- in a non-bold sans-serif style wrapped in a 'Just' data constructor. For
-- numbers outside this range, 'Nothing' is returned.
intToDigitSansSerifRegular ::
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in sans-serifs not in bold wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
intToDigitSansSerifRegular = _withCondition _isValidInt intToDigitSansSerifRegular'

-- | Convert the given number (@0@–@9@) to its corresponding character
-- in a bold sans-serif style. The result for numbers outside this range is
-- unspecified.
intToDigitSansSerifBold' ::
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in sans-serifs in bold, unspecified outside the the range.
  Char
intToDigitSansSerifBold' = digitSansSerifBold' . intToDigit

-- | Convert the given number (@0@–@9@) to its corresponding character
-- in a bold sans-serif style wrapped in a 'Just' data constructor. For
-- numbers outside this range, 'Nothing' is returned.
intToDigitSansSerifBold ::
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in sans-serifs in bold wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
intToDigitSansSerifBold = _withCondition _isValidInt intToDigitSansSerifBold'

-- | Convert the given number (@0@–@9@) to its corresponding character
-- with a given 'Emphasis' in sans-serif style. The result for numbers outside this
-- range is unspecified.
intToDigitSansSerif' ::
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in sans-serifs in the given /emphasis/ style, unspecified outside the the range.
  Char
intToDigitSansSerif' = splitEmphasis intToDigitSansSerifRegular' intToDigitSansSerifBold'

-- | Convert the given number (@0@–@9@) to its corresponding character
-- with the given 'Emphasis' in sans-serif style wrapped in a 'Just' data constructor.
-- For numbers outside this range, 'Nothing' is returned.
intToDigitSansSerif ::
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given number to convert
  Int ->
  -- | The corresponding symbol in sans-serifs in the given /emphasis/ style wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
intToDigitSansSerif = splitEmphasis intToDigitSansSerifRegular intToDigitSansSerifBold

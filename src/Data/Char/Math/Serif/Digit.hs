{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math.Serif.Digit
-- Description : Serif mathematical alphanumeric symbols
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- See "Data.Char.Math" for further documentation.
module Data.Char.Math.Serif.Digit
  ( -- * Characters conversion
    digitSerif,
    digitSerif',
    digitSerifRegular,
    digitSerifRegular',
    digitSerifBold,
    digitSerifBold',

    -- ** Int to digit characters
    intToDigitSerif,
    intToDigitSerif',
    intToDigitSerifRegular,
    intToDigitSerifRegular',
    intToDigitSerifBold,
    intToDigitSerifBold',
  )
where

import Data.Char (intToDigit, isDigit)
import Data.Char.Core (Emphasis, splitEmphasis)
import Data.Char.Math.Internal

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- with a given 'Emphasis' in serif style. The result for characters outside this
-- range is unspecified.
digitSerif' ::
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in serifs for the given /emphasis/ style, unspecified outside the the range.
  Char
digitSerif' = splitEmphasis digitSerifRegular' digitSerifBold'

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- with the given 'Emphasis' in serif style wrapped in a 'Just' data constructor.
-- For characters outside this range, 'Nothing' is returned.
digitSerif ::
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in serifs for the given /emphasis/ style wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
digitSerif = splitEmphasis digitSerifRegular digitSerifBold

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- in a non-bold serif style. The result for characters outside this range is
-- unspecified.
digitSerifRegular' ::
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in serifs not in bold, unspecified outside the the range.
  Char
digitSerifRegular' = id

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- in a non-bold serif style wrapped in a 'Just' data constructor. For
-- characters outside this range, 'Nothing' is returned.
digitSerifRegular ::
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in serifs not in bold wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
digitSerifRegular = _withCondition isDigit digitSerifRegular'

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- in a bold serif style. The result for characters outside this range is
-- unspecified.
digitSerifBold' ::
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in serifs in bold, unspecified outside the the range.
  Char
digitSerifBold' = _shiftC 0x1d79e

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- in a bold serif style wrapped in a 'Just' data constructor. For
-- characters outside this range, 'Nothing' is returned.
digitSerifBold ::
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in serifs in bold wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
digitSerifBold = _withCondition isDigit digitSerifBold'

-- | Convert the given number (@0@–@9@) to its corresponding character
-- in a non-bold serif style. The result for numbers outside this range is
-- unspecified.
intToDigitSerifRegular' ::
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in serifs not in bold, unspecified outside the the range.
  Char
intToDigitSerifRegular' = digitSerifRegular' . intToDigit

-- | Convert the given number (@0@–@9@) to its corresponding character
-- in a non-bold serif style wrapped in a 'Just' data constructor. For
-- numbers outside this range, 'Nothing' is returned.
intToDigitSerifRegular ::
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in serifs not in bold wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
intToDigitSerifRegular = _withCondition _isValidInt intToDigitSerifRegular'

-- | Convert the given number (@0@–@9@) to its corresponding character
-- in a bold serif style. The result for numbers outside this range is
-- unspecified.
intToDigitSerifBold' ::
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in serifs in bold, unspecified outside the the range.
  Char
intToDigitSerifBold' = digitSerifBold' . intToDigit

-- | Convert the given number (@0@–@9@) to its corresponding character
-- in a bold serif style wrapped in a 'Just' data constructor. For
-- numbers outside this range, 'Nothing' is returned.
intToDigitSerifBold ::
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in serifs in bold wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
intToDigitSerifBold = _withCondition _isValidInt intToDigitSerifBold'

-- | Convert the given number (@0@–@9@) to its corresponding character
-- with a given 'Emphasis' in serif style. The result for numbers outside this
-- range is unspecified.
intToDigitSerif' ::
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in serifs in the given /emphasis/ style, unspecified outside the the range.
  Char
intToDigitSerif' = splitEmphasis intToDigitSerifRegular' intToDigitSerifBold'

-- | Convert the given number (@0@–@9@) to its corresponding character
-- with the given 'Emphasis' in serif style wrapped in a 'Just' data constructor.
-- For numbers outside this range, 'Nothing' is returned.
intToDigitSerif ::
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given number to convert
  Int ->
  -- | The corresponding symbol in serifs in the given /emphasis/ style wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
intToDigitSerif = splitEmphasis intToDigitSerifRegular intToDigitSerifBold

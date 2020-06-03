{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math.SansSerif.Digit
Description : Sans serif mathematical alphanumeric symbols
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Data.Char.Math.SansSerif.Digit
  ( -- * Character conversion
    digitSansSerif,        digitSansSerif'
  , digitSansSerifRegular, digitSansSerifRegular'
  , digitSansSerifBold,    digitSansSerifBold'
    -- * Int to digit characters
  , intToDigitSansSerif,        intToDigitSansSerif'
  , intToDigitSansSerifRegular, intToDigitSansSerifRegular'
  , intToDigitSansSerifBold,    intToDigitSansSerifBold'
  ) where


import Data.Char (intToDigit, isDigit)
import Data.Char.Core(Emphasis, splitEmphasis)
import Data.Char.Math.Internal


-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- in a non-bold sans-serif style. The result for characters outside this range is
-- unspecified.
digitSansSerifRegular'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The corresponding symbol in sans-serifs not in bold, unspecified outside the the range.
digitSansSerifRegular' = _shiftC 0x1d7b2

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- in a non-bold sans-serif style wrapped in a 'Just' data constructor. For
-- characters outside this range, 'Nothing' is returned.
digitSansSerifRegular
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The corresponding symbol in sans-serifs not in bold wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
digitSansSerifRegular = _withCondition isDigit digitSansSerifRegular'

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- in a bold sans-serif style. The result for characters outside this range is
-- unspecified.
digitSansSerifBold'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The corresponding symbol in sans-serifs in bold, unspecified outside the the range.
digitSansSerifBold' = _shiftC 0x1d7bc

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- in a bold sans-serif style wrapped in a 'Just' data constructor. For
-- characters outside this range, 'Nothing' is returned.
digitSansSerifBold
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The corresponding symbol in sans-serifs in bold wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
digitSansSerifBold = _withCondition isDigit digitSansSerifBold'

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- with a given 'Emphasis' in sans-serif style. The result for characters outside this
-- range is unspecified.
digitSansSerif'
  :: Emphasis  -- ^ The given /emphasis/ style.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The corresponding symbol in sans-serifs for the given /emphasis/ style, unspecified outside the the range.
digitSansSerif' = splitEmphasis digitSansSerifRegular' digitSansSerifBold'

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- with the given 'Emphasis' in sans-serif style wrapped in a 'Just' data constructor.
-- For characters outside this range, 'Nothing' is returned.
digitSansSerif
  :: Emphasis  -- ^ The given /emphasis/ style.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The corresponding symbol in sans-serifs for the given /emphasis/ style wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
digitSansSerif = splitEmphasis digitSansSerifRegular digitSansSerifBold

-- | Convert the given number (@0@-@9@) to its corresponding character
-- in a non-bold sans-serif style. The result for numbers outside this range is
-- unspecified.
intToDigitSansSerifRegular'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding symbol in sans-serifs not in bold, unspecified outside the the range.
intToDigitSansSerifRegular' = digitSansSerifRegular' . intToDigit

-- | Convert the given number (@0@-@9@) to its corresponding character
-- in a non-bold sans-serif style wrapped in a 'Just' data constructor. For
-- numbers outside this range, 'Nothing' is returned.
intToDigitSansSerifRegular
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding symbol in sans-serifs not in bold wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
intToDigitSansSerifRegular = _withCondition _isValidInt intToDigitSansSerifRegular'

-- | Convert the given number (@0@-@9@) to its corresponding character
-- in a bold sans-serif style. The result for numbers outside this range is
-- unspecified.
intToDigitSansSerifBold'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding symbol in sans-serifs in bold, unspecified outside the the range.
intToDigitSansSerifBold' = digitSansSerifBold' . intToDigit

-- | Convert the given number (@0@-@9@) to its corresponding character
-- in a bold sans-serif style wrapped in a 'Just' data constructor. For
-- numbers outside this range, 'Nothing' is returned.
intToDigitSansSerifBold
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding symbol in sans-serifs in bold wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
intToDigitSansSerifBold = _withCondition _isValidInt intToDigitSansSerifBold'

-- | Convert the given number (@0@-@9@) to its corresponding character
-- with a given 'Emphasis' in sans-serif style. The result for numbers outside this
-- range is unspecified.
intToDigitSansSerif'
  :: Emphasis  -- ^ The given /emphasis/ style.
  -> Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding symbol in sans-serifs in the given /emphasis/ style, unspecified outside the the range.
intToDigitSansSerif' = splitEmphasis intToDigitSansSerifRegular' intToDigitSansSerifBold'

-- | Convert the given number (@0@-@9@) to its corresponding character
-- with the given 'Emphasis' in sans-serif style wrapped in a 'Just' data constructor.
-- For numbers outside this range, 'Nothing' is returned.
intToDigitSansSerif
  :: Emphasis  -- ^ The given /emphasis/ style.
  -> Int  -- ^ The given number to convert
  -> Maybe Char  -- ^ The corresponding symbol in sans-serifs in the given /emphasis/ style wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
intToDigitSansSerif = splitEmphasis intToDigitSansSerifRegular intToDigitSansSerifBold
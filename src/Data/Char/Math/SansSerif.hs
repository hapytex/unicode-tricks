{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math
Description : Sans serif mathematical alphanumeric symbols
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Data.Char.Math.SansSerif
  ( -- * Sans-serif mathematical alphabet symbols
    sansSerif,               sansSerif'
  , sansSerifNoBold,         sansSerifNoBold'
  , sansSerifBold,           sansSerifBold'
  , sansSerifNoItalic,       sansSerifNoItalic'
  , sansSerifItalic,         sansSerifItalic'
  , sansSerifNoBoldNoItalic, sansSerifNoBoldNoItalic'
  , sansSerifBoldNoItalic,   sansSerifBoldNoItalic'
  , sansSerifNoBoldItalic,   sansSerifNoBoldItalic'
  , sansSerifBoldItalic,     sansSerifBoldItalic'
    -- * Digit characters
  , digitSansSerif,        digitSansSerif'
  , digitSansSerifRegular, digitSansSerifRegular'
  , digitSansSerifBold,    digitSansSerifBold'
    -- ** Int to digit characters
  , intToDigitSansSerif,        intToDigitSansSerif'
  , intToDigitSansSerifRegular, intToDigitSansSerifRegular'
  , intToDigitSansSerifBold,    intToDigitSansSerifBold'
  ) where


import Data.Char (intToDigit, isDigit)
import Data.Char.Core(
    Emphasis, ItalicType
  , isAsciiAlpha, splitEmphasis, splitItalicType
  )
import Data.Char.Math.Internal


-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the character is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerif
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerif = splitItalicType sansSerifNoItalic sansSerifItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerif'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
sansSerif' = splitItalicType sansSerifNoItalic' sansSerifItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and no /italics/. This maps characters to itself for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifNoBoldNoItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, not in bold and not in italics.
sansSerifNoBoldNoItalic' = _baseUpperLower 0x1d559

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifNoBoldNoItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifNoBoldNoItalic = _withCondition isAsciiAlpha sansSerifNoBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and in /italics/. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifNoBoldItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, not in bold and in italics.
sansSerifNoBoldItalic' = _baseUpperLower 0x1d5c1

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifNoBoldItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifNoBoldItalic = _withCondition isAsciiAlpha sansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- not in /italics/. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifBoldNoItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, in bold and not in italics.
sansSerifBoldNoItalic' = _baseUpperLower 0x1d58d

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifBoldNoItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifBoldNoItalic = _withCondition isAsciiAlpha sansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifBoldItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, in bold and in italics.
sansSerifBoldItalic' = _baseUpperLower 0x1d5f5

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifBoldItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifBoldItalic = _withCondition isAsciiAlpha sansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifBold'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, in bold and depending on the given 'ItalicType' in italics or not.
sansSerifBold' = splitItalicType sansSerifBoldNoItalic' sansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifBold
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifBold = splitItalicType sansSerifBoldNoItalic sansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifNoBold'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, not in bold and depending on the given 'ItalicType' in italics or not.
sansSerifNoBold' = splitItalicType sansSerifNoBoldNoItalic' sansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifNoBold
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifNoBold = splitItalicType sansSerifNoBoldNoItalic sansSerifNoBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifItalic'
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and in italics.
sansSerifItalic' = splitEmphasis sansSerifNoBoldItalic' sansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'. If the character
-- is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifItalic
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifItalic = splitEmphasis sansSerifNoBoldItalic sansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifNoItalic'
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
sansSerifNoItalic' = splitEmphasis sansSerifNoBoldNoItalic' sansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'. If the character
-- is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifNoItalic
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifNoItalic = splitEmphasis sansSerifNoBoldNoItalic sansSerifBoldNoItalic

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
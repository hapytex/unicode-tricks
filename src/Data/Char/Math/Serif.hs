{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math.Serif
Description : Serif mathematical alphanumeric symbols
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Data.Char.Math.Serif
  ( -- * Serif mathematical alphabet symbols
    serif,               serif'
  , serifNoBold,         serifNoBold'
  , serifBold,           serifBold'
  , serifNoItalic,       serifNoItalic'
  , serifItalic,         serifItalic'
  , serifNoBoldNoItalic, serifNoBoldNoItalic'
  , serifBoldNoItalic,   serifBoldNoItalic'
  , serifNoBoldItalic,   serifNoBoldItalic'
  , serifBoldItalic,     serifBoldItalic'
    -- * Digit characters
  , digitSerif,            digitSerif'
  , digitSerifRegular,     digitSerifRegular'
  , digitSerifBold,        digitSerifBold'
    -- ** Int to digit characters
  , intToDigitSerif,            intToDigitSerif'
  , intToDigitSerifRegular,     intToDigitSerifRegular'
  , intToDigitSerifBold,        intToDigitSerifBold'
  ) where


import Data.Char (intToDigit, isDigit)
import Data.Char.Core (Emphasis, ItalicType, isAsciiAlpha, splitEmphasis, splitItalicType)
import Data.Char.Math.Internal


-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the character is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serif
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serif = splitItalicType serifNoItalic serifItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serif'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
serif' = splitItalicType serifNoItalic' serifItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/ and no /italics/. This maps characters to itself for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifNoBoldNoItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, not in bold and not in italics.
serifNoBoldNoItalic' = id

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifNoBoldNoItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifNoBoldNoItalic = _withCondition isAsciiAlpha serifNoBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/ and in /italics/. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifNoBoldItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, not in bold and in italics.
serifNoBoldItalic' 'h' = '\x210e'
serifNoBoldItalic' c = _baseUpperLower 0x1d3ed c

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifNoBoldItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifNoBoldItalic = _withCondition isAsciiAlpha serifNoBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- not in /italics/. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifBoldNoItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, in bold and not in italics.
serifBoldNoItalic' = _baseUpperLower 0x1d3b9

-- | Convert the given character to a mathematical symbol with serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifBoldNoItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifBoldNoItalic = _withCondition isAsciiAlpha serifBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifBoldItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, in bold and in italics.
serifBoldItalic' = _baseUpperLower 0x1d421

-- | Convert the given character to a mathematical symbol with serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifBoldItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifBoldItalic = _withCondition isAsciiAlpha serifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifBold'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, in bold and depending on the given 'ItalicType' in italics or not.
serifBold' = splitItalicType serifBoldNoItalic' serifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifBold
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifBold = splitItalicType serifBoldNoItalic serifBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifNoBold'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, not in bold and depending on the given 'ItalicType' in italics or not.
serifNoBold' = splitItalicType serifNoBoldNoItalic' serifNoBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'. If the character is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifNoBold
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifNoBold = splitItalicType serifNoBoldNoItalic serifNoBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifItalic'
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and in italics.
serifItalic' = splitEmphasis serifNoBoldItalic' serifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'. If the character
-- is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifItalic
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifItalic = splitEmphasis serifNoBoldItalic serifBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifNoItalic'
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
serifNoItalic' = splitEmphasis serifNoBoldNoItalic' serifBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'. If the character
-- is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifNoItalic
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifNoItalic = splitEmphasis serifNoBoldNoItalic serifBoldNoItalic

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- in a non-bold serif style. The result for characters outside this range is
-- unspecified.
digitSerifRegular'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The corresponding symbol in serifs not in bold, unspecified outside the the range.
digitSerifRegular' = id

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- in a non-bold serif style wrapped in a 'Just' data constructor. For
-- characters outside this range, 'Nothing' is returned.
digitSerifRegular
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The corresponding symbol in serifs not in bold wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
digitSerifRegular = _withCondition isDigit digitSerifRegular'

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- in a bold serif style. The result for characters outside this range is
-- unspecified.
digitSerifBold'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The corresponding symbol in serifs in bold, unspecified outside the the range.
digitSerifBold' = _shiftC 0x1d79e

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- in a bold serif style wrapped in a 'Just' data constructor. For
-- characters outside this range, 'Nothing' is returned.
digitSerifBold
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The corresponding symbol in serifs in bold wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
digitSerifBold = _withCondition isDigit digitSerifBold'

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- with a given 'Emphasis' in serif style. The result for characters outside this
-- range is unspecified.
digitSerif'
  :: Emphasis  -- ^ The given /emphasis/ style.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The corresponding symbol in serifs for the given /emphasis/ style, unspecified outside the the range.
digitSerif' = splitEmphasis digitSerifRegular' digitSerifBold'

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- with the given 'Emphasis' in serif style wrapped in a 'Just' data constructor.
-- For characters outside this range, 'Nothing' is returned.
digitSerif
  :: Emphasis  -- ^ The given /emphasis/ style.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The corresponding symbol in serifs for the given /emphasis/ style wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
digitSerif = splitEmphasis digitSerifRegular digitSerifBold

-- | Convert the given number (@0@-@9@) to its corresponding character
-- in a non-bold serif style. The result for numbers outside this range is
-- unspecified.
intToDigitSerifRegular'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding symbol in serifs not in bold, unspecified outside the the range.
intToDigitSerifRegular' = digitSerifRegular' . intToDigit

-- | Convert the given number (@0@-@9@) to its corresponding character
-- in a non-bold serif style wrapped in a 'Just' data constructor. For
-- numbers outside this range, 'Nothing' is returned.
intToDigitSerifRegular
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding symbol in serifs not in bold wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
intToDigitSerifRegular = _withCondition _isValidInt intToDigitSerifRegular'

-- | Convert the given number (@0@-@9@) to its corresponding character
-- in a bold serif style. The result for numbers outside this range is
-- unspecified.
intToDigitSerifBold'
  :: Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding symbol in serifs in bold, unspecified outside the the range.
intToDigitSerifBold' = digitSerifBold' . intToDigit

-- | Convert the given number (@0@-@9@) to its corresponding character
-- in a bold serif style wrapped in a 'Just' data constructor. For
-- numbers outside this range, 'Nothing' is returned.
intToDigitSerifBold
  :: Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding symbol in serifs in bold wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
intToDigitSerifBold = _withCondition _isValidInt intToDigitSerifBold'

-- | Convert the given number (@0@-@9@) to its corresponding character
-- with a given 'Emphasis' in serif style. The result for numbers outside this
-- range is unspecified.
intToDigitSerif'
  :: Emphasis  -- ^ The given /emphasis/ style.
  -> Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding symbol in serifs in the given /emphasis/ style, unspecified outside the the range.
intToDigitSerif' = splitEmphasis intToDigitSerifRegular' intToDigitSerifBold'

-- | Convert the given number (@0@-@9@) to its corresponding character
-- with the given 'Emphasis' in serif style wrapped in a 'Just' data constructor.
-- For numbers outside this range, 'Nothing' is returned.
intToDigitSerif
  :: Emphasis  -- ^ The given /emphasis/ style.
  -> Int  -- ^ The given number to convert
  -> Maybe Char  -- ^ The corresponding symbol in serifs in the given /emphasis/ style wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
intToDigitSerif = splitEmphasis intToDigitSerifRegular intToDigitSerifBold
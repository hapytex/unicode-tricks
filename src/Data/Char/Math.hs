{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math
Description : A module to write math unicode alphanumerical characters.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module aims to make it more convenient to transform mathematical characters
to their /double struck/, /fraktur/, /calligraphic/, etc. equivalent.

Most of the characters are defined in the <https://www.unicode.org/charts/PDF/U1D400.pdf 1d400>
unicode block. See also the <https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols Wikipedia page>.
-}

module Data.Char.Math (
    -- * Mathematical symbols
    math, math'
    -- * Serif mathematical symbols
  , serif,               serif'
  , serifNoBold,         serifNoBold'
  , serifBold,           serifBold'
  , serifNoItalic,       serifNoItalic'
  , serifItalic,         serifItalic'
  , serifNoBoldNoItalic, serifNoBoldNoItalic'
  , serifBoldNoItalic,   serifBoldNoItalic'
  , serifNoBoldItalic,   serifNoBoldItalic'
  , serifBoldItalic,     serifBoldItalic'
    -- * Sans-serif mathematical symbols
  , sansSerif,               sansSerif'
  , sansSerifNoBold,         sansSerifNoBold'
  , sansSerifBold,           sansSerifBold'
  , sansSerifNoItalic,       sansSerifNoItalic'
  , sansSerifItalic,         sansSerifItalic'
  , sansSerifNoBoldNoItalic, sansSerifNoBoldNoItalic'
  , sansSerifBoldNoItalic,   sansSerifBoldNoItalic'
  , sansSerifNoBoldItalic,   sansSerifNoBoldItalic'
  , sansSerifBoldItalic,     sansSerifBoldItalic'
    -- * Latin characters
  , latin
  , mathAlpha, mathAlpha'
    -- * Digit characters
    -- ** Char-based conversion
  , digit,                 digit'
  , digitSansSerif,        digitSansSerif'
  , digitSerif,            digitSerif'
  , digitSerifRegular,     digitSerifRegular'
  , digitSerifBold,        digitSerifBold'
  , digitSansSerifRegular, digitSansSerifRegular'
  , digitSansSerifBold,    digitSansSerifBold'
  , digitMonospace,        digitMonospace'
  , digitDoubleStruck,     digitDoubleStruck'
    -- ** Int to digit characters
  , intToDigitChar,             intToDigitChar'
  , intToDigitSerif,            intToDigitSerif'
  , intToDigitSansSerif,        intToDigitSansSerif'
  , intToDigitSerifRegular,     intToDigitSerifRegular'
  , intToDigitSerifBold,        intToDigitSerifBold'
  , intToDigitMonospace,        intToDigitMonospace'
  , intToDigitDoubleStruck,     intToDigitDoubleStruck'
    -- * Monospace symbols
  , monospace, monospace'
    -- * Double struck symbols
  , doubleStruck, doubleStruck'
    -- * Script (or calligraphic symbols)
  , script,             script'
  , scriptRegular,      scriptRegular'
  , scriptBold,         scriptBold'
  , calligraphy,        calligraphy'
  , calligraphyRegular, calligraphyRegular'
  , calligraphyBold,    calligraphyBold'
    -- * Fraktur symbols
  , fraktur,        fraktur'
  , frakturRegular, frakturRegular'
  , frakturBold,    frakturBold'
  ) where

import Data.Char.Core (Emphasis, FontStyle, ItalicType, splitFontStyle, isAsciiAlpha)
import Data.Char.Math.DoubleStruck
import Data.Char.Math.Fraktur
import Data.Char.Math.Monospace
import Data.Char.Math.SansSerif
import Data.Char.Math.Script
import Data.Char.Math.Serif

-- | Convert the given character to a mathematical symbol with the given /font/ style, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent sans-serif symbol for:
-- * ASCII latin letters: @A@-@Z@ and @a@-@z@ ranges;
-- * greek-like symbols: @"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ"@;
-- * digit: @0-9@ range.
-- For characters outside the range, the behavior is unspecified.
math'
  :: FontStyle  -- ^ The given 'FontStyle' to use.
  -> ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted in the given 'FontStyle', depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
math' = splitFontStyle sansSerif' serif'

-- | Convert the given character to a mathematical symbol with the given /font/
-- style, in the given /emphasis/ and in the given /italics/ type wrapped in a 'Just'.
-- The supported ranges are:
-- * ASCII latin letters: @A@-@Z@ and @a@-@z@ ranges;
-- * greek-like symbols: @"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ"@;
-- * digit: @0-9@ range.
-- If the character is outside theses ranges, 'Nothing' is returned.
math
  :: FontStyle  -- ^ The given 'FontStyle' to use.
  -> ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
math = splitFontStyle sansSerif serif

{-# DEPRECATED mathAlpha' "Use `math'`" #-}
-- | Convert the given character to a mathematical symbol with the given /font/ style, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
mathAlpha'
  :: FontStyle  -- ^ The given 'FontStyle' to use.
  -> ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted in the given 'FontStyle', depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
mathAlpha' = math'

{-# DEPRECATED mathAlpha "Use `latin`" #-}
-- | Convert the given character to a mathematical symbol with the given /font/
-- style, in the given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the character is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
mathAlpha
  :: FontStyle  -- ^ The given 'FontStyle' to use.
  -> ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
mathAlpha = latin

-- | Convert the given character to a mathematical symbol with the given /font/
-- style, in the given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the character is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
latin
  :: FontStyle  -- ^ The given 'FontStyle' to use.
  -> ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latin f i e c
  | isAsciiAlpha c = Just $ splitFontStyle sansSerif' serif' f i e c
  | otherwise      = Nothing

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- with a given 'Emphasis' in the given /font/ style. The result for characters outside this
-- range is unspecified.
digit'
  :: FontStyle  -- ^ The given /font/ style.
  -> Emphasis  -- ^ The given /emphasis/ style.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The corresponding symbol in the given /font/ style for the given /emphasis/ style, unspecified outside the the range.
digit' = splitFontStyle digitSansSerif' digitSerif'

-- | Convert the given digit character (@0@-@9@) to its corresponding character
-- with the given 'Emphasis' in the given /font/ style wrapped in a 'Just' data constructor.
-- For characters outside this range, 'Nothing' is returned.
digit
  :: FontStyle  -- ^ The given /font/ style.
  -> Emphasis  -- ^ The given /emphasis/ style.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The corresponding symbol in serifs for the given /emphasis/ style wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
digit = splitFontStyle digitSansSerif digitSerif

-- | Convert the given number (@0@-@9@) to its corresponding character
-- with a given 'Emphasis' in the given 'FontStyle'. The result for numbers outside this
-- range is unspecified.
intToDigitChar'
  :: FontStyle  -- ^ The given /font/ style.
  -> Emphasis  -- ^ The given /emphasis/ style.
  -> Int  -- ^ The given number to convert.
  -> Char  -- ^ The corresponding symbol in sans-serifs in the given /font/ style the given /emphasis/ style, unspecified outside the the range.
intToDigitChar' = splitFontStyle intToDigitSansSerif' intToDigitSerif'

-- | Convert the given number (@0@-@9@) to its corresponding character
-- with the given 'Emphasis' in the given 'FontStyle' wrapped in a 'Just' data constructor.
-- For numbers outside this range, 'Nothing' is returned.
intToDigitChar
  :: FontStyle  -- ^ The given /font/ style.
  -> Emphasis  -- ^ The given /emphasis/ style.
  -> Int  -- ^ The given number to convert.
  -> Maybe Char  -- ^ The corresponding symbol in the given /font/ style in the given /emphasis/ style wrapped in a 'Just',
                -- 'Nothing' if the character is outside the range.
intToDigitChar = splitFontStyle intToDigitSansSerif intToDigitSerif

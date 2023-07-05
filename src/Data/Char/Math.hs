{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math
-- Description : A module to write math unicode alphanumerical characters.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- = Introduction
-- This module aims to make it more convenient to transform mathematical characters
-- to their /double struck/, /fraktur/, /calligraphic/, etc. equivalent.
--
-- Most of the characters are defined in the @<https://www.unicode.org/charts/PDF/U1D400.pdf 1D400–1D7FF>@
-- Unicode block /Mathematical Alphanumeric Symbols/.
-- See also the <https://en.wikipedia.org/wiki/Mathematical_Alphanumeric_Symbols Wikipedia page>.
--
-- == Examples
-- >>> math Serif Italic Bold 'x'
-- Just '\119961'
-- >>> math Serif Italic Bold '3'
-- Just '\120785'
-- >>> latinMath Serif Italic Bold 'x'
-- Just '\119961'
-- >>> latinMath Serif Italic Bold '3'
-- Nothing
-- >>> script NoBold 'S'
-- Just '\119982'
-- >>> intToDigitChar SansSerif Bold 3
-- Just '\120815'
-- >>> intToDigitChar SansSerif Bold 33
-- Nothing
--
-- == Supported ranges of characters #characters_ranges#
-- The transformations of this module only supports the following small subset of Unicode points:
--
-- [ASCII latin letters] @A@–@Z@ and @a@–@z@ ranges
-- [Greek-like symbols]
--
--     * The following characters from the Unicode block [/Greek and Coptic/](http://unicode.org/charts/PDF/U0370.pdf):
--       @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩϴαβγδεζηθικλμνξοπρςστυφχψωϵϑϰϕϱϖ@.
--     * The following characters from the Unicode block [/Mathematical Operators/](https://www.unicode.org/charts/PDF/U2200.pdf):
--       @∇∂@.
--
-- [Digits] @0@–@9@ range
--
-- == Naming Conventions
-- The functions with a name finishing with a quote @'@ (such as 'math'') do not check their input:
-- they transform the characters in the supported range and have an /unspecified behaviour/ outside this range.
--
-- The functions without a quote in their name (such as 'math') check their input and output and wrap the
-- resulting transformation with 'Maybe'.
module Data.Char.Math
  ( -- * Serif/sans-serif mathematical symbols
    math,
    math',

    -- ** Latin-only characters
    latinMath,
    mathAlpha,
    mathAlpha',

    -- ** Serif mathematical symbols
    serif,
    serif',
    serifNoBold,
    serifNoBold',
    serifBold,
    serifBold',
    serifNoItalic,
    serifNoItalic',
    serifItalic,
    serifItalic',
    serifNoBoldNoItalic,
    serifNoBoldNoItalic',
    serifBoldNoItalic,
    serifBoldNoItalic',
    serifNoBoldItalic,
    serifNoBoldItalic',
    serifBoldItalic,
    serifBoldItalic',

    -- ** Sans-serif mathematical symbols
    sansSerif,
    sansSerif',
    sansSerifNoBold,
    sansSerifNoBold',
    sansSerifBold,
    sansSerifBold',
    sansSerifNoItalic,
    sansSerifNoItalic',
    sansSerifItalic,
    sansSerifItalic',
    sansSerifNoBoldNoItalic,
    sansSerifNoBoldNoItalic',
    sansSerifBoldNoItalic,
    sansSerifBoldNoItalic',
    sansSerifNoBoldItalic,
    sansSerifNoBoldItalic',
    sansSerifBoldItalic,
    sansSerifBoldItalic',

    -- * Digit characters

    -- ** Char-based conversion
    digit,
    digit',
    digitSansSerif,
    digitSansSerif',
    digitSerif,
    digitSerif',
    digitSerifRegular,
    digitSerifRegular',
    digitSerifBold,
    digitSerifBold',
    digitSansSerifRegular,
    digitSansSerifRegular',
    digitSansSerifBold,
    digitSansSerifBold',
    digitMonospace,
    digitMonospace',
    digitDoubleStruck,
    digitDoubleStruck',

    -- ** Int to digit characters
    intToDigitChar,
    intToDigitChar',
    intToDigitSerif,
    intToDigitSerif',
    intToDigitSansSerif,
    intToDigitSansSerif',
    intToDigitSerifRegular,
    intToDigitSerifRegular',
    intToDigitSerifBold,
    intToDigitSerifBold',
    intToDigitMonospace,
    intToDigitMonospace',
    intToDigitDoubleStruck,
    intToDigitDoubleStruck',

    -- * Monospace symbols
    monospace,
    monospace',

    -- * Double struck symbols
    doubleStruck,
    doubleStruck',

    -- * Script (or calligraphic symbols)
    script,
    script',
    scriptRegular,
    scriptRegular',
    scriptBold,
    scriptBold',
    calligraphy,
    calligraphy',
    calligraphyRegular,
    calligraphyRegular',
    calligraphyBold,
    calligraphyBold',

    -- * Fraktur symbols
    fraktur,
    fraktur',
    frakturRegular,
    frakturRegular',
    frakturBold,
    frakturBold',
  )
where

import Data.Char.Core (Emphasis, FontStyle, ItalicType, isAsciiAlpha, splitFontStyle)
import Data.Char.Math.DoubleStruck
import Data.Char.Math.Fraktur
import Data.Char.Math.Monospace
import Data.Char.Math.SansSerif
import Data.Char.Math.Script
import Data.Char.Math.Serif

-- | Convert the given character to a mathematical symbol with the given /font/ style, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent sans-serif symbol for
-- characters in the [supported ranges](#characters_ranges).
-- For characters outside the range, the behavior is unspecified.
math' ::
  -- | The given 'FontStyle' to use.
  FontStyle ->
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted in the given 'FontStyle', depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
  Char
math' = splitFontStyle sansSerif' serif'

-- | Convert the given character to a mathematical symbol with the given /font/
-- style, in the given /emphasis/ and in the given /italics/ type wrapped in a 'Just'
-- if the character is supported (see: [supported ranges](#characters_ranges))
-- If the character is outside theses ranges, 'Nothing' is returned.
math ::
  -- | The given 'FontStyle' to use.
  FontStyle ->
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
math = splitFontStyle sansSerif serif

{-# DEPRECATED mathAlpha' "Use `math'`" #-}

-- | Convert the given character to a mathematical symbol with the given /font/ style, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent sansSerif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
mathAlpha' ::
  -- | The given 'FontStyle' to use.
  FontStyle ->
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted in the given 'FontStyle', depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
  Char
mathAlpha' = math'

{-# DEPRECATED mathAlpha "Use `latinMath`" #-}

-- | Convert the given character to a mathematical symbol with the given /font/
-- style, in the given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the character is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
mathAlpha ::
  -- | The given 'FontStyle' to use.
  FontStyle ->
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
mathAlpha = latinMath

-- | Convert the given character to a mathematical symbol with the given /font/
-- style, in the given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the character is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinMath ::
  -- | The given 'FontStyle' to use.
  FontStyle ->
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
latinMath f i e c
  | isAsciiAlpha c = Just $ splitFontStyle sansSerif' serif' f i e c
  | otherwise = Nothing

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- with a given 'Emphasis' in the given /font/ style. The result for characters outside this
-- range is unspecified.
digit' ::
  -- | The given /font/ style.
  FontStyle ->
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in the given /font/ style for the given /emphasis/ style, unspecified outside the the range.
  Char
digit' = splitFontStyle digitSansSerif' digitSerif'

-- | Convert the given digit character (@0@–@9@) to its corresponding character
-- with the given 'Emphasis' in the given /font/ style wrapped in a 'Just' data constructor.
-- For characters outside this range, 'Nothing' is returned.
digit ::
  -- | The given /font/ style.
  FontStyle ->
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The corresponding symbol in serifs for the given /emphasis/ style wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
digit = splitFontStyle digitSansSerif digitSerif

-- | Convert the given number (@0@–@9@) to its corresponding character
-- with a given 'Emphasis' in the given 'FontStyle'. The result for numbers outside this
-- range is unspecified.
intToDigitChar' ::
  -- | The given /font/ style.
  FontStyle ->
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in sans-serifs in the given /font/ style the given /emphasis/ style, unspecified outside the the range.
  Char
intToDigitChar' = splitFontStyle intToDigitSansSerif' intToDigitSerif'

-- | Convert the given number (@0@–@9@) to its corresponding character
-- with the given 'Emphasis' in the given 'FontStyle' wrapped in a 'Just' data constructor.
-- For numbers outside this range, 'Nothing' is returned.
intToDigitChar ::
  -- | The given /font/ style.
  FontStyle ->
  -- | The given /emphasis/ style.
  Emphasis ->
  -- | The given number to convert.
  Int ->
  -- | The corresponding symbol in the given /font/ style in the given /emphasis/ style wrapped in a 'Just',
  -- 'Nothing' if the character is outside the range.
  Maybe Char
intToDigitChar = splitFontStyle intToDigitSansSerif intToDigitSerif

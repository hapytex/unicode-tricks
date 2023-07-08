{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math.SansSerif
-- Description : Sans serif mathematical alphanumeric symbols
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- See "Data.Char.Math" for further documentation.
module Data.Char.Math.SansSerif
  ( -- * Sans-serif mathematical alphabet symbols
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

    -- ** Character conversion
    digitSansSerif,
    digitSansSerif',
    digitSansSerifRegular,
    digitSansSerifRegular',
    digitSansSerifBold,
    digitSansSerifBold',

    -- ** Int to digit characters
    intToDigitSansSerif,
    intToDigitSansSerif',
    intToDigitSansSerifRegular,
    intToDigitSansSerifRegular',
    intToDigitSansSerifBold,
    intToDigitSansSerifBold',
  )
where

import Data.Char (isDigit)
import Data.Char.Core (Emphasis, ItalicType, isAsciiAlpha, splitEmphasis, splitItalicType)
import Data.Char.Math.Internal (_dispatchLatinGreekDigit, _dispatchLatinGreekDigit')
import Data.Char.Math.SansSerif.Digit
import Data.Char.Math.SansSerif.Greek
import Data.Char.Math.SansSerif.Latin

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
sansSerif ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
sansSerif = splitItalicType sansSerifNoItalic sansSerifItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and a given /italics/ style.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
sansSerif' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
  Char
sansSerif' = splitItalicType sansSerifNoItalic' sansSerifItalic'

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and in italics.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
sansSerifItalic' ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and in italics.
  Char
sansSerifItalic' = splitEmphasis sansSerifNoBoldItalic' sansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
sansSerifItalic ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
sansSerifItalic = splitEmphasis sansSerifNoBoldItalic sansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and not in italics.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
sansSerifNoItalic' ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
  Char
sansSerifNoItalic' = splitEmphasis sansSerifNoBoldNoItalic' sansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
sansSerifNoItalic ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
sansSerifNoItalic = splitEmphasis sansSerifNoBoldNoItalic sansSerifBoldNoItalic

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- and in a /italics/ type.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
sansSerifBold' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, in bold and depending on the given 'ItalicType' in italics or not.
  Char
sansSerifBold' = splitItalicType sansSerifBoldNoItalic' sansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
sansSerifBold ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
sansSerifBold = splitItalicType sansSerifBoldNoItalic sansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, not in /bold/
-- and in a /italics/ type.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
sansSerifNoBold' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, not in bold and depending on the given 'ItalicType' in italics or not.
  Char
sansSerifNoBold' = splitItalicType sansSerifNoBoldNoItalic' sansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
sansSerifNoBold ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
sansSerifNoBold = splitItalicType sansSerifNoBoldNoItalic sansSerifNoBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and no /italics/.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
sansSerifNoBoldNoItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, not in bold and not in italics.
  Char
sansSerifNoBoldNoItalic' = _dispatchLatinGreekDigit' latinSansSerifNoBoldNoItalic' greekSansSerifNoBoldNoItalic' digitSansSerifRegular'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and no /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
sansSerifNoBoldNoItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
sansSerifNoBoldNoItalic c
  | isAsciiAlpha c = Just (latinSansSerifNoBoldNoItalic' c)
  | isDigit c = Just (digitSansSerifRegular' c)
  | otherwise = Nothing

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and in /italics/.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
sansSerifNoBoldItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, not in bold and in italics.
  Char
sansSerifNoBoldItalic' = _dispatchLatinGreekDigit' latinSansSerifNoBoldItalic' greekSansSerifNoBoldItalic' digitSansSerifRegular'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
sansSerifNoBoldItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
sansSerifNoBoldItalic c
  | isAsciiAlpha c = Just (latinSansSerifNoBoldItalic' c)
  | isDigit c = Just (digitSansSerifRegular' c)
  | otherwise = Nothing

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- not in /italics/.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
sansSerifBoldNoItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, in bold and not in italics.
  Char
sansSerifBoldNoItalic' = _dispatchLatinGreekDigit' latinSansSerifBoldNoItalic' greekSansSerifBoldNoItalic' digitSansSerifBold'

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
sansSerifBoldNoItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
sansSerifBoldNoItalic = _dispatchLatinGreekDigit latinSansSerifBoldNoItalic' greekSansSerifBoldNoItalic' digitSansSerifBold'

-- | Convert the given character to a mathematical symbol without serifs, with in
-- /bold/ and in /italics/.
-- If the symbol is not supported (see: "Data.Char.Math#characters_ranges"), the returned character is unspecified.
sansSerifBoldItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, in bold and in italics.
  Char
sansSerifBoldItalic' = _dispatchLatinGreekDigit' latinSansSerifBoldItalic' greekSansSerifBoldItalic' digitSansSerifBold'

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
sansSerifBoldItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
sansSerifBoldItalic = _dispatchLatinGreekDigit latinSansSerifBoldItalic' greekSansSerifBoldItalic' digitSansSerifBold'

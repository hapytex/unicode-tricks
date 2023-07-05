{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math.SansSerif.Latin
-- Description : Sans serif mathematical alphanumeric symbols
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- See "Data.Char.Math" for further documentation.
module Data.Char.Math.SansSerif.Latin
  ( latinSansSerif,
    latinSansSerif',
    latinSansSerifNoBold,
    latinSansSerifNoBold',
    latinSansSerifBold,
    latinSansSerifBold',
    latinSansSerifNoItalic,
    latinSansSerifNoItalic',
    latinSansSerifItalic,
    latinSansSerifItalic',
    latinSansSerifNoBoldNoItalic,
    latinSansSerifNoBoldNoItalic',
    latinSansSerifBoldNoItalic,
    latinSansSerifBoldNoItalic',
    latinSansSerifNoBoldItalic,
    latinSansSerifNoBoldItalic',
    latinSansSerifBoldItalic,
    latinSansSerifBoldItalic',
  )
where

import Data.Char.Core
  ( Emphasis,
    ItalicType,
    isAsciiAlpha,
    splitEmphasis,
    splitItalicType,
  )
import Data.Char.Math.Internal

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the character is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerif ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
latinSansSerif = splitItalicType latinSansSerifNoItalic latinSansSerifItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerif' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
  Char
latinSansSerif' = splitItalicType latinSansSerifNoItalic' latinSansSerifItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and no /italics/. This maps characters to itself for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifNoBoldNoItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, not in bold and not in italics.
  Char
latinSansSerifNoBoldNoItalic' = _baseUpperLower 0x1d559

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifNoBoldNoItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
latinSansSerifNoBoldNoItalic = _withCondition isAsciiAlpha latinSansSerifNoBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and in /italics/. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifNoBoldItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, not in bold and in italics.
  Char
latinSansSerifNoBoldItalic' = _baseUpperLower 0x1d5c1

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifNoBoldItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
latinSansSerifNoBoldItalic = _withCondition isAsciiAlpha latinSansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- not in /italics/. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifBoldNoItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, in bold and not in italics.
  Char
latinSansSerifBoldNoItalic' = _baseUpperLower 0x1d58d

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifBoldNoItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
latinSansSerifBoldNoItalic = _withCondition isAsciiAlpha latinSansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifBoldItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, in bold and in italics.
  Char
latinSansSerifBoldItalic' = _baseUpperLower 0x1d5f5

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifBoldItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
latinSansSerifBoldItalic = _withCondition isAsciiAlpha latinSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifBold' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, in bold and depending on the given 'ItalicType' in italics or not.
  Char
latinSansSerifBold' = splitItalicType latinSansSerifBoldNoItalic' latinSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifBold ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
latinSansSerifBold = splitItalicType latinSansSerifBoldNoItalic latinSansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifNoBold' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, not in bold and depending on the given 'ItalicType' in italics or not.
  Char
latinSansSerifNoBold' = splitItalicType latinSansSerifNoBoldNoItalic' latinSansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifNoBold ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
latinSansSerifNoBold = splitItalicType latinSansSerifNoBoldNoItalic latinSansSerifNoBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifItalic' ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and in italics.
  Char
latinSansSerifItalic' = splitEmphasis latinSansSerifNoBoldItalic' latinSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'. If the character
-- is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifItalic ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
latinSansSerifItalic = splitEmphasis latinSansSerifNoBoldItalic latinSansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifNoItalic' ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
  Char
latinSansSerifNoItalic' = splitEmphasis latinSansSerifNoBoldNoItalic' latinSansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'. If the character
-- is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifNoItalic ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
latinSansSerifNoItalic = splitEmphasis latinSansSerifNoBoldNoItalic latinSansSerifBoldNoItalic

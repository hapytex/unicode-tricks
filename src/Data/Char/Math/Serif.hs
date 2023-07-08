{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math.Serif
-- Description : Serif mathematical alphanumeric symbols
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- See "Data.Char.Math" for further documentation.
module Data.Char.Math.Serif
  ( -- * Serif mathematical alphabet symbols
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

    -- * Digit characters
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

import Data.Char.Core (Emphasis, ItalicType, splitEmphasis, splitItalicType)
import Data.Char.Math.Internal (_dispatchLatinGreekDigit, _dispatchLatinGreekDigit')
import Data.Char.Math.Serif.Digit
import Data.Char.Math.Serif.Greek
import Data.Char.Math.Serif.Latin

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
serif ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
serif = splitItalicType serifNoItalic serifItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent serif symbol for
-- supported characters (see: "Data.Char.Math#characters_ranges").
-- For characters outside the range, the behavior is unspecified.
serif' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
  Char
serif' = splitItalicType serifNoItalic' serifItalic'

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent serif symbol for
-- supported characters (see: "Data.Char.Math#characters_ranges").
-- For characters outside the range, the behavior is unspecified.
serifNoItalic' ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
  Char
serifNoItalic' = splitEmphasis serifNoBoldNoItalic' serifBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
serifNoItalic ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
serifNoItalic = splitEmphasis serifNoBoldNoItalic serifBoldNoItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent serif symbol for
-- supported characters (see: "Data.Char.Math#characters_ranges").
-- For characters outside the range, the behavior is unspecified.
serifItalic' ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and in italics.
  Char
serifItalic' = splitEmphasis serifNoBoldItalic' serifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
serifItalic ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
serifItalic = splitEmphasis serifNoBoldItalic serifBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent serif symbol for
-- supported characters (see: "Data.Char.Math#characters_ranges").
-- For characters outside the range, the behavior is unspecified.
serifBold' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, in bold and depending on the given 'ItalicType' in italics or not.
  Char
serifBold' = splitItalicType serifBoldNoItalic' serifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
serifBold ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
serifBold = splitItalicType serifBoldNoItalic serifBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent serif symbol for
-- supported characters (see: "Data.Char.Math#characters_ranges").
-- For characters outside the range, the behavior is unspecified.
serifNoBold' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, not in bold and depending on the given 'ItalicType' in italics or not.
  Char
serifNoBold' = splitItalicType serifNoBoldNoItalic' serifNoBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
serifNoBold ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
serifNoBold = splitItalicType serifNoBoldNoItalic serifNoBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/ and no /italics/. This maps characters to itself for
-- supported characters (see: "Data.Char.Math#characters_ranges").
-- For characters outside the range, the behavior is unspecified.
serifNoBoldNoItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, not in bold and not in italics.
  Char
serifNoBoldNoItalic' = id

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/, and no /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
serifNoBoldNoItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
serifNoBoldNoItalic = _dispatchLatinGreekDigit latinSerifNoBoldNoItalic' greekSerifNoBoldNoItalic' digitSerifRegular'

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/ and in /italics/. This maps characters an equivalent serif symbol for
-- supported characters (see: "Data.Char.Math#characters_ranges").
-- For characters outside the range, the behavior is unspecified.
serifNoBoldItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, not in bold and in italics.
  Char
serifNoBoldItalic' = _dispatchLatinGreekDigit' latinSerifNoBoldItalic' greekSerifNoBoldItalic' digitSerifRegular'

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
serifNoBoldItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
serifNoBoldItalic = _dispatchLatinGreekDigit latinSerifNoBoldItalic' greekSerifNoBoldItalic' digitSerifRegular'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- not in /italics/. This maps characters an equivalent serif symbol for
-- supported characters (see: "Data.Char.Math#characters_ranges").
-- For characters outside the range, the behavior is unspecified.
serifBoldNoItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, in bold and not in italics.
  Char
serifBoldNoItalic' = _dispatchLatinGreekDigit' latinSerifBoldNoItalic' greekSerifBoldNoItalic' digitSerifBold'

-- | Convert the given character to a mathematical symbol with serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
serifBoldNoItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
serifBoldNoItalic = _dispatchLatinGreekDigit latinSerifBoldNoItalic' greekSerifBoldNoItalic' digitSerifBold'

-- | Convert the given character to a mathematical symbol with serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent serif symbol for
-- supported characters (see: "Data.Char.Math#characters_ranges").
-- For characters outside the range, the behavior is unspecified.
serifBoldItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, in bold and in italics.
  Char
serifBoldItalic' = _dispatchLatinGreekDigit' latinSerifBoldItalic' greekSerifBoldItalic' digitSerifBold'

-- | Convert the given character to a mathematical symbol with serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'.
-- If the character is not supported (see: "Data.Char.Math#characters_ranges"), 'Nothing' is returned.
serifBoldItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
serifBoldItalic = _dispatchLatinGreekDigit latinSerifBoldItalic' greekSerifBoldItalic' digitSerifBold'

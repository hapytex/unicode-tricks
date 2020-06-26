{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math.Serif.Latin
Description : Serif mathematical alphanumeric symbols
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

See "Data.Char.Math" for further documentation.
-}

module Data.Char.Math.Serif.Latin
  ( latinSerif,               latinSerif'
  , latinSerifNoBold,         latinSerifNoBold'
  , latinSerifBold,           latinSerifBold'
  , latinSerifNoItalic,       latinSerifNoItalic'
  , latinSerifItalic,         latinSerifItalic'
  , latinSerifNoBoldNoItalic, latinSerifNoBoldNoItalic'
  , latinSerifBoldNoItalic,   latinSerifBoldNoItalic'
  , latinSerifNoBoldItalic,   latinSerifNoBoldItalic'
  , latinSerifBoldItalic,     latinSerifBoldItalic'
  ) where


import Data.Char.Core (Emphasis, ItalicType, isAsciiAlpha, splitEmphasis, splitItalicType)
import Data.Char.Math.Internal


-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the character is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSerif
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSerif = splitItalicType latinSerifNoItalic latinSerifItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSerif'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
latinSerif' = splitItalicType latinSerifNoItalic' latinSerifItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/ and no /italics/. This maps characters to itself for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSerifNoBoldNoItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, not in bold and not in italics.
latinSerifNoBoldNoItalic' = id

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSerifNoBoldNoItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSerifNoBoldNoItalic = _withCondition isAsciiAlpha latinSerifNoBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/ and in /italics/. This maps characters an equivalent serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSerifNoBoldItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, not in bold and in italics.
latinSerifNoBoldItalic' 'h' = '\x210e'
latinSerifNoBoldItalic' c = _baseUpperLower 0x1d3ed c

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSerifNoBoldItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSerifNoBoldItalic = _withCondition isAsciiAlpha latinSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- not in /italics/. This maps characters an equivalent serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSerifBoldNoItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, in bold and not in italics.
latinSerifBoldNoItalic' = _baseUpperLower 0x1d3b9

-- | Convert the given character to a mathematical symbol with serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSerifBoldNoItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSerifBoldNoItalic = _withCondition isAsciiAlpha latinSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSerifBoldItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, in bold and in italics.
latinSerifBoldItalic' = _baseUpperLower 0x1d421

-- | Convert the given character to a mathematical symbol with serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSerifBoldItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSerifBoldItalic = _withCondition isAsciiAlpha latinSerifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSerifBold'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, in bold and depending on the given 'ItalicType' in italics or not.
latinSerifBold' = splitItalicType latinSerifBoldNoItalic' latinSerifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSerifBold
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSerifBold = splitItalicType latinSerifBoldNoItalic latinSerifBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSerifNoBold'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, not in bold and depending on the given 'ItalicType' in italics or not.
latinSerifNoBold' = splitItalicType latinSerifNoBoldNoItalic' latinSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSerifNoBold
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSerifNoBold = splitItalicType latinSerifNoBoldNoItalic latinSerifNoBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSerifItalic'
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and in italics.
latinSerifItalic' = splitEmphasis latinSerifNoBoldItalic' latinSerifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'. If the character
-- is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSerifItalic
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSerifItalic = splitEmphasis latinSerifNoBoldItalic latinSerifBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSerifNoItalic'
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
latinSerifNoItalic' = splitEmphasis latinSerifNoBoldNoItalic' latinSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'. If the character
-- is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSerifNoItalic
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSerifNoItalic = splitEmphasis latinSerifNoBoldNoItalic latinSerifBoldNoItalic

{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math.SansSerif.Latin
Description : Sans serif mathematical alphanumeric symbols
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

See "Data.Char.Math" for further documentation.
-}

module Data.Char.Math.SansSerif.Latin
  ( latinSansSerif,               latinSansSerif'
  , latinSansSerifNoBold,         latinSansSerifNoBold'
  , latinSansSerifBold,           latinSansSerifBold'
  , latinSansSerifNoItalic,       latinSansSerifNoItalic'
  , latinSansSerifItalic,         latinSansSerifItalic'
  , latinSansSerifNoBoldNoItalic, latinSansSerifNoBoldNoItalic'
  , latinSansSerifBoldNoItalic,   latinSansSerifBoldNoItalic'
  , latinSansSerifNoBoldItalic,   latinSansSerifNoBoldItalic'
  , latinSansSerifBoldItalic,     latinSansSerifBoldItalic'
  ) where


import Data.Char.Core(
    Emphasis, ItalicType
  , isAsciiAlpha, splitEmphasis, splitItalicType
  )
import Data.Char.Math.Internal


-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the character is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerif
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSansSerif = splitItalicType latinSansSerifNoItalic latinSansSerifItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerif'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
latinSansSerif' = splitItalicType latinSansSerifNoItalic' latinSansSerifItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and no /italics/. This maps characters to itself for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifNoBoldNoItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, not in bold and not in italics.
latinSansSerifNoBoldNoItalic' = _baseUpperLower 0x1d559

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifNoBoldNoItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSansSerifNoBoldNoItalic = _withCondition isAsciiAlpha latinSansSerifNoBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and in /italics/. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifNoBoldItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, not in bold and in italics.
latinSansSerifNoBoldItalic' = _baseUpperLower 0x1d5c1

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifNoBoldItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSansSerifNoBoldItalic = _withCondition isAsciiAlpha latinSansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- not in /italics/. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifBoldNoItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, in bold and not in italics.
latinSansSerifBoldNoItalic' = _baseUpperLower 0x1d58d

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifBoldNoItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSansSerifBoldNoItalic = _withCondition isAsciiAlpha latinSansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifBoldItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, in bold and in italics.
latinSansSerifBoldItalic' = _baseUpperLower 0x1d5f5

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifBoldItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSansSerifBoldItalic = _withCondition isAsciiAlpha latinSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifBold'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, in bold and depending on the given 'ItalicType' in italics or not.
latinSansSerifBold' = splitItalicType latinSansSerifBoldNoItalic' latinSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifBold
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSansSerifBold = splitItalicType latinSansSerifBoldNoItalic latinSansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifNoBold'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, not in bold and depending on the given 'ItalicType' in italics or not.
latinSansSerifNoBold' = splitItalicType latinSansSerifNoBoldNoItalic' latinSansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'. If the character is outside the
-- @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifNoBold
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSansSerifNoBold = splitItalicType latinSansSerifNoBoldNoItalic latinSansSerifNoBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifItalic'
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and in italics.
latinSansSerifItalic' = splitEmphasis latinSansSerifNoBoldItalic' latinSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'. If the character
-- is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifItalic
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSansSerifItalic = splitEmphasis latinSansSerifNoBoldItalic latinSansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent sans-serif symbol for the @A@–@Z@ and
-- @a@–@z@ range. For characters outside the range, the behavior is unspecified.
latinSansSerifNoItalic'
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
latinSansSerifNoItalic' = splitEmphasis latinSansSerifNoBoldNoItalic' latinSansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'. If the character
-- is outside the @A@–@Z@ and @a@–@z@ range, 'Nothing' is returned.
latinSansSerifNoItalic
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
latinSansSerifNoItalic = splitEmphasis latinSansSerifNoBoldNoItalic latinSansSerifBoldNoItalic

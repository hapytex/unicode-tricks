{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math.SansSerif.Latin
Description : Sans serif mathematical alphanumeric symbols
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Data.Char.Math.SansSerif.Latin
  ( sansSerif,               sansSerif'
  , sansSerifNoBold,         sansSerifNoBold'
  , sansSerifBold,           sansSerifBold'
  , sansSerifNoItalic,       sansSerifNoItalic'
  , sansSerifItalic,         sansSerifItalic'
  , sansSerifNoBoldNoItalic, sansSerifNoBoldNoItalic'
  , sansSerifBoldNoItalic,   sansSerifBoldNoItalic'
  , sansSerifNoBoldItalic,   sansSerifNoBoldItalic'
  , sansSerifBoldItalic,     sansSerifBoldItalic'
  ) where


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

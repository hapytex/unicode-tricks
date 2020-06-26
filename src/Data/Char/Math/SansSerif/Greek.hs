{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math.SansSerif.Greek
Description : Sans serif mathematical alphanumeric symbols
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

See "Data.Char.Math" for further documentation.
-}

module Data.Char.Math.SansSerif.Greek
  ( greekSansSerif,               greekSansSerif'
  , greekSansSerifNoBold,         greekSansSerifNoBold'
  , greekSansSerifBold,           greekSansSerifBold'
  , greekSansSerifNoItalic,       greekSansSerifNoItalic'
  , greekSansSerifItalic,         greekSansSerifItalic'
  , greekSansSerifNoBoldNoItalic, greekSansSerifNoBoldNoItalic'
  , greekSansSerifBoldNoItalic,   greekSansSerifBoldNoItalic'
  , greekSansSerifNoBoldItalic,   greekSansSerifNoBoldItalic'
  , greekSansSerifBoldItalic,     greekSansSerifBoldItalic'
  ) where


import Data.Char.Core (Emphasis, ItalicType, isGreek, splitEmphasis, splitItalicType)
import Data.Char.Math.Internal


-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerif
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
greekSansSerif = splitItalicType greekSansSerifNoItalic greekSansSerifItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerif'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
greekSansSerif' = splitItalicType greekSansSerifNoItalic' greekSansSerifItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and no /italics/. This maps characters to itself.
greekSansSerifNoBoldNoItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, not in bold and not in italics.
greekSansSerifNoBoldNoItalic' = id

greekSansSerifNoBoldNoItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
greekSansSerifNoBoldNoItalic = const Nothing

greekSansSerifNoBoldItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, not in bold and in italics.
greekSansSerifNoBoldItalic' = greekSansSerifNoBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifNoBoldItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
greekSansSerifNoBoldItalic = greekSansSerifNoBoldNoItalic

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- not in /italics/. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifBoldNoItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, in bold and not in italics.
greekSansSerifBoldNoItalic' 'ϴ' = '𝝧'
greekSansSerifBoldNoItalic' '∇' = '𝝯'
greekSansSerifBoldNoItalic' '∂' = '𝞉'
greekSansSerifBoldNoItalic' 'ϵ' = '𝞊'
greekSansSerifBoldNoItalic' 'ϑ' = '𝞋'
greekSansSerifBoldNoItalic' 'ϰ' = '𝞌'
greekSansSerifBoldNoItalic' 'ϕ' = '𝞍'
greekSansSerifBoldNoItalic' 'ϱ' = '𝞎'
greekSansSerifBoldNoItalic' 'ϖ' = '𝞏'
greekSansSerifBoldNoItalic' c
  | 'Α' <= c && c <= 'Ω' = _baseUpperLower 0x1d3c5 c
  | 'α' <= c && c <= 'ω' = _baseUpperLower 0x1d3bf c
  | otherwise            = c

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifBoldNoItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
greekSansSerifBoldNoItalic = _withCondition isGreek greekSansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifBoldItalic'
  :: Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, in bold and in italics.
greekSansSerifBoldItalic' 'ϴ' = '𝞡'
greekSansSerifBoldItalic' '∇' = '𝞩'
greekSansSerifBoldItalic' '∂' = '𝟃'
greekSansSerifBoldItalic' 'ϵ' = '𝟄'
greekSansSerifBoldItalic' 'ϑ' = '𝟅'
greekSansSerifBoldItalic' 'ϰ' = '𝟆'
greekSansSerifBoldItalic' 'ϕ' = '𝟇'
greekSansSerifBoldItalic' 'ϱ' = '𝟈'
greekSansSerifBoldItalic' 'ϖ' = '𝟉'
greekSansSerifBoldItalic' c
  | 'Α' <= c && c <= 'Ω' = _baseUpperLower 0x1d3ff c
  | 'α' <= c && c <= 'ω' = _baseUpperLower 0x1d3f9 c
  | otherwise            = c

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifBoldItalic
  :: Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
greekSansSerifBoldItalic = _withCondition isGreek greekSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifBold'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, in bold and depending on the given 'ItalicType' in italics or not.
greekSansSerifBold' = splitItalicType greekSansSerifBoldNoItalic' greekSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifBold
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
greekSansSerifBold = splitItalicType greekSansSerifBoldNoItalic greekSansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifNoBold'
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, not in bold and depending on the given 'ItalicType' in italics or not.
greekSansSerifNoBold' = splitItalicType greekSansSerifNoBoldNoItalic' greekSansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifNoBold
  :: ItalicType  -- ^ The given 'ItalicType' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
greekSansSerifNoBold = splitItalicType greekSansSerifNoBoldNoItalic greekSansSerifNoBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifItalic'
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and in italics.
greekSansSerifItalic' = splitEmphasis greekSansSerifNoBoldItalic' greekSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifItalic
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
greekSansSerifItalic = splitEmphasis greekSansSerifNoBoldItalic greekSansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifNoItalic'
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Char  -- ^ The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
greekSansSerifNoItalic' = splitEmphasis greekSansSerifNoBoldNoItalic' greekSansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifNoItalic
  :: Emphasis  -- ^ The given 'Emphasis' to use.
  -> Char  -- ^ The given character to convert.
  -> Maybe Char  -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
greekSansSerifNoItalic = splitEmphasis greekSansSerifNoBoldNoItalic greekSansSerifBoldNoItalic

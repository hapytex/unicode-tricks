{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math.SansSerif.Greek
-- Description : Sans serif mathematical alphanumeric symbols
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- See "Data.Char.Math" for further documentation.
module Data.Char.Math.SansSerif.Greek
  ( greekSansSerif,
    greekSansSerif',
    greekSansSerifNoBold,
    greekSansSerifNoBold',
    greekSansSerifBold,
    greekSansSerifBold',
    greekSansSerifNoItalic,
    greekSansSerifNoItalic',
    greekSansSerifItalic,
    greekSansSerifItalic',
    greekSansSerifNoBoldNoItalic,
    greekSansSerifNoBoldNoItalic',
    greekSansSerifBoldNoItalic,
    greekSansSerifBoldNoItalic',
    greekSansSerifNoBoldItalic,
    greekSansSerifNoBoldItalic',
    greekSansSerifBoldItalic,
    greekSansSerifBoldItalic',
  )
where

import Data.Char.Core (Emphasis, ItalicType, isGreek, splitEmphasis, splitItalicType)
import Data.Char.Math.Internal

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerif ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSansSerif = splitItalicType greekSansSerifNoItalic greekSansSerifItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerif' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
  Char
greekSansSerif' = splitItalicType greekSansSerifNoItalic' greekSansSerifItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and no /italics/. This maps characters to itself.
greekSansSerifNoBoldNoItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, not in bold and not in italics.
  Char
greekSansSerifNoBoldNoItalic' = id

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and no /italics/. This maps characters to itself wrapped in a 'Just';
-- 'Nothing' if that character does not exists.
greekSansSerifNoBoldNoItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSansSerifNoBoldNoItalic = const Nothing

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and no /italics/. This maps characters to itself.
greekSansSerifNoBoldItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, not in bold and in italics.
  Char
greekSansSerifNoBoldItalic' = greekSansSerifNoBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifNoBoldItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSansSerifNoBoldItalic = greekSansSerifNoBoldNoItalic

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- not in /italics/. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifBoldNoItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, in bold and not in italics.
  Char
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
  | otherwise = c

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifBoldNoItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSansSerifBoldNoItalic = _withCondition isGreek greekSansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifBoldItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, in bold and in italics.
  Char
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
  | otherwise = c

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifBoldItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSansSerifBoldItalic = _withCondition isGreek greekSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifBold' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, in bold and depending on the given 'ItalicType' in italics or not.
  Char
greekSansSerifBold' = splitItalicType greekSansSerifBoldNoItalic' greekSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifBold ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSansSerifBold = splitItalicType greekSansSerifBoldNoItalic greekSansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifNoBold' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, not in bold and depending on the given 'ItalicType' in italics or not.
  Char
greekSansSerifNoBold' = splitItalicType greekSansSerifNoBoldNoItalic' greekSansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifNoBold ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSansSerifNoBold = splitItalicType greekSansSerifNoBoldNoItalic greekSansSerifNoBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifItalic' ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and in italics.
  Char
greekSansSerifItalic' = splitEmphasis greekSansSerifNoBoldItalic' greekSansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifItalic ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSansSerifItalic = splitEmphasis greekSansSerifNoBoldItalic greekSansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent sans-serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSansSerifNoItalic' ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted without serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
  Char
greekSansSerifNoItalic' = splitEmphasis greekSansSerifNoBoldNoItalic' greekSansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSansSerifNoItalic ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSansSerifNoItalic = splitEmphasis greekSansSerifNoBoldNoItalic greekSansSerifBoldNoItalic

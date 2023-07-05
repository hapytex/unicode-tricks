{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Math.Serif.Greek
-- Description : Serif mathematical alphanumeric symbols
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- See "Data.Char.Math" for further documentation.
module Data.Char.Math.Serif.Greek
  ( greekSerif,
    greekSerif',
    greekSerifNoBold,
    greekSerifNoBold',
    greekSerifBold,
    greekSerifBold',
    greekSerifNoItalic,
    greekSerifNoItalic',
    greekSerifItalic,
    greekSerifItalic',
    greekSerifNoBoldNoItalic,
    greekSerifNoBoldNoItalic',
    greekSerifBoldNoItalic,
    greekSerifBoldNoItalic',
    greekSerifNoBoldItalic,
    greekSerifNoBoldItalic',
    greekSerifBoldItalic,
    greekSerifBoldItalic',
  )
where

import Data.Char.Core (Emphasis, ItalicType, isGreek, splitEmphasis, splitItalicType)
import Data.Char.Math.Internal

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSerif ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSerif = splitItalicType greekSerifNoItalic greekSerifItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSerif' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
  Char
greekSerif' = splitItalicType greekSerifNoItalic' greekSerifItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/ and no /italics/. This maps characters to itself.
greekSerifNoBoldNoItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, not in bold and not in italics.
  Char
greekSerifNoBoldNoItalic' = id

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSerifNoBoldNoItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSerifNoBoldNoItalic = _withCondition isGreek greekSerifNoBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/ and in /italics/. This maps characters an equivalent serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSerifNoBoldItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, not in bold and in italics.
  Char
greekSerifNoBoldItalic' 'ϴ' = '𝛳'
greekSerifNoBoldItalic' '∇' = '𝛻'
greekSerifNoBoldItalic' '∂' = '𝜕'
greekSerifNoBoldItalic' 'ϵ' = '𝜖'
greekSerifNoBoldItalic' 'ϑ' = '𝜗'
greekSerifNoBoldItalic' 'ϰ' = '𝜘'
greekSerifNoBoldItalic' 'ϕ' = '𝜙'
greekSerifNoBoldItalic' 'ϱ' = '𝜚'
greekSerifNoBoldItalic' 'ϖ' = '𝜛'
greekSerifNoBoldItalic' c
  | 'Α' <= c && c <= 'Ω' = _baseUpperLower 0x1d351 c
  | 'α' <= c && c <= 'ω' = _baseUpperLower 0x1d34b c
  | otherwise = c

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSerifNoBoldItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSerifNoBoldItalic = _withCondition isGreek greekSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- not in /italics/. This maps characters an equivalent serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSerifBoldNoItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, in bold and not in italics.
  Char
greekSerifBoldNoItalic' 'ϴ' = '𝚹'
greekSerifBoldNoItalic' '∇' = '𝛁'
greekSerifBoldNoItalic' '∂' = '𝛛'
greekSerifBoldNoItalic' 'ϵ' = '𝛜'
greekSerifBoldNoItalic' 'ϑ' = '𝛝'
greekSerifBoldNoItalic' 'ϰ' = '𝛞'
greekSerifBoldNoItalic' 'ϕ' = '𝛟'
greekSerifBoldNoItalic' 'ϱ' = '𝛠'
greekSerifBoldNoItalic' 'ϖ' = '𝛡'
greekSerifBoldNoItalic' c
  | 'Α' <= c && c <= 'Ω' = _baseUpperLower 0x1d317 c
  | 'α' <= c && c <= 'ω' = _baseUpperLower 0x1d311 c
  | otherwise = c

-- | Convert the given character to a mathematical symbol with serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSerifBoldNoItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSerifBoldNoItalic = _withCondition isGreek greekSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSerifBoldItalic' ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, in bold and in italics.
  Char
greekSerifBoldItalic' 'ϴ' = '𝜭'
greekSerifBoldItalic' '∇' = '𝜵'
greekSerifBoldItalic' '∂' = '𝝏'
greekSerifBoldItalic' 'ϵ' = '𝝐'
greekSerifBoldItalic' 'ϑ' = '𝝑'
greekSerifBoldItalic' 'ϰ' = '𝝒'
greekSerifBoldItalic' 'ϕ' = '𝝓'
greekSerifBoldItalic' 'ϱ' = '𝝔'
greekSerifBoldItalic' 'ϖ' = '𝝕'
greekSerifBoldItalic' c
  | 'Α' <= c && c <= 'Ω' = _baseUpperLower 0x1d38b c
  | 'α' <= c && c <= 'ω' = _baseUpperLower 0x1d385 c
  | otherwise = c

-- | Convert the given character to a mathematical symbol with serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSerifBoldItalic ::
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSerifBoldItalic = _withCondition isGreek greekSerifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSerifBold' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, in bold and depending on the given 'ItalicType' in italics or not.
  Char
greekSerifBold' = splitItalicType greekSerifBoldNoItalic' greekSerifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSerifBold ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSerifBold = splitItalicType greekSerifBoldNoItalic greekSerifBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSerifNoBold' ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, not in bold and depending on the given 'ItalicType' in italics or not.
  Char
greekSerifNoBold' = splitItalicType greekSerifNoBoldNoItalic' greekSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSerifNoBold ::
  -- | The given 'ItalicType' to use.
  ItalicType ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSerifNoBold = splitItalicType greekSerifNoBoldNoItalic greekSerifNoBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSerifItalic' ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and in italics.
  Char
greekSerifItalic' = splitEmphasis greekSerifNoBoldItalic' greekSerifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSerifItalic ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSerifItalic = splitEmphasis greekSerifNoBoldItalic greekSerifBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent serif symbol
-- for the characters in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@.
-- For characters outside the range, the behavior is unspecified.
greekSerifNoItalic' ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character that is formatted with serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
  Char
greekSerifNoItalic' = splitEmphasis greekSerifNoBoldNoItalic' greekSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'. If the character
-- is not in @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@, 'Nothing' is returned.
greekSerifNoItalic ::
  -- | The given 'Emphasis' to use.
  Emphasis ->
  -- | The given character to convert.
  Char ->
  -- | The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
  Maybe Char
greekSerifNoItalic = splitEmphasis greekSerifNoBoldNoItalic greekSerifBoldNoItalic

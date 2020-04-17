{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math
Description : A module to write math unicode alphanumerical characters.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module aims to make it more convenient to transform mathematical characters
to their /double struck/, /fraktur/, /calligraphic/, etc. equivalent.
-}

module Data.Char.Math (
    -- * Serif mathematical alphanumerical symbols
    serifNoBoldNoItalic, serifNoBoldNoItalic'
  , serifBoldNoItalic,   serifBoldNoItalic'
  , serifNoBoldItalic,   serifNoBoldItalic'
  , serifBoldItalic,     serifBoldItalic'
  , serifNoBold,         serifNoBold'
  , serifBold,           serifBold'
  , serifNoItalic,       serifNoItalic'
  , serifItalic,         serifItalic'
  , serif,               serif'
    -- * Sans-serif mathematical alphanumerical symbols
  , sansSerifNoBoldNoItalic, sansSerifNoBoldNoItalic'
  , sansSerifBoldNoItalic,   sansSerifBoldNoItalic'
  , sansSerifNoBoldItalic,   sansSerifNoBoldItalic'
  , sansSerifBoldItalic,     sansSerifBoldItalic'
  , sansSerifNoBold,         sansSerifNoBold'
  , sansSerifBold,           sansSerifBold'
  , sansSerifNoItalic,       sansSerifNoItalic'
  , sansSerifItalic,         sansSerifItalic'
  , sansSerif,               sansSerif'
    -- * Monospace symbols
  , monospace, monospace'
    -- * Double struck symbols
  , doubleStruck, doubleStruck'
    -- * Script (or calligraphic symbols)
  , script,             script'
  , scriptRegular,      scriptRegular'
  , scriptBold,         scriptBold'
  , calligraphy,        calligraphy'
  , calligraphyRegular, calligraphyRegular'
  , calligraphyBold,    calligraphyBold'
    -- * Fraktur symbols
  , fraktur,        fraktur'
  , frakturRegular, frakturRegular'
  , frakturBold,    frakturBold'
  ) where

import Data.Char(chr, isAsciiUpper, isDigit, ord)
import Data.Char.Core(Emphasis(NoBold, Bold), FontStyle(SansSerif, Serif), ItalicType(NoItalic, Italic), isAsciiAlpha, isAsciiAlphaNum)

_boldSplit :: (a -> b) -> (a -> b) -> Emphasis -> a -> b
_boldSplit fnb fb = go
    where go NoBold = fnb
          go Bold = fb

_italicSplit :: (a -> b) -> (a -> b) -> ItalicType -> a -> b
_italicSplit fni fi = go
    where go NoItalic = fni
          go Italic = fi

_fontSplit :: (a -> b) -> (a -> b) -> FontStyle -> a -> b
_fontSplit fss fs = go
    where go SansSerif = fss
          go Serif = fs

_ordc :: Char -> Int -> Char
_ordc = (chr .) . (+) . ord

_baseUpperLower :: Int -> Char -> Char
_baseUpperLower b c
    | isAsciiUpper c = oc (b+6)
    | otherwise = oc b
    where oc = _ordc c

_baseUpperLowerNum :: Int -> Int -> Char -> Char
_baseUpperLowerNum n b c
    | isDigit c = oc n
    | isAsciiUpper c = oc (b+6)
    | otherwise = oc b
    where oc = _ordc c

_withCondition :: (Char -> Bool) -> (Char -> Char) -> Char -> Maybe Char
_withCondition p f = go
    where go x | p x = Just (f x)
               | otherwise = Nothing

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/ and no /italics/. This maps characters to itself for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifNoBoldNoItalic'
  :: Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated with serifs, not in bold and not in italics.
serifNoBoldNoItalic' = id

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/, and no /italics/ wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifNoBoldNoItalic
  :: Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifNoBoldNoItalic = _withCondition isAsciiAlpha serifNoBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/ and in /italics/. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifNoBoldItalic'
  :: Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated with serifs, not in bold and in italics.
serifNoBoldItalic' 'h' = '\x210e'
serifNoBoldItalic' c = _baseUpperLower 0x1d3ed c

-- | Convert the given character to a mathematical symbol with serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifNoBoldItalic
  :: Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifNoBoldItalic = _withCondition isAsciiAlpha serifNoBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- not in /italics/. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifBoldNoItalic'
  :: Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated with serifs, in bold and not in italics.
serifBoldNoItalic' = _baseUpperLower 0x1d3b9

-- | Convert the given character to a mathematical symbol with serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifBoldNoItalic
  :: Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifBoldNoItalic = _withCondition isAsciiAlpha serifBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifBoldItalic'
  :: Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated with serifs, in bold and in italics.
serifBoldItalic' = _baseUpperLower 0x1d421

-- | Convert the given character to a mathematical symbol with serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifBoldItalic
  :: Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifBoldItalic = _withCondition isAsciiAlpha serifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifBold'
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated with serifs, in bold and depending on the given 'ItalicType' in italics or not.
serifBold' = _italicSplit serifBoldNoItalic' serifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifBold
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifBold = _italicSplit serifBoldNoItalic serifBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifNoBold'
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated with serifs, not in bold and depending on the given 'ItalicType' in italics or not.
serifNoBold' = _italicSplit serifNoBoldNoItalic' serifNoBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifNoBold
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifNoBold = _italicSplit serifNoBoldNoItalic serifNoBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifItalic'
  :: Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated with serifs, depending on the given 'Emphasis' in bold or not, and in italics.
serifItalic' = _boldSplit serifNoBoldItalic' serifBoldItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'. If the charcter
-- is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifItalic
  :: Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifItalic = _boldSplit serifNoBoldItalic serifBoldItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serifNoItalic'
  :: Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated with serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
serifNoItalic' = _boldSplit serifNoBoldNoItalic' serifBoldNoItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'. If the charcter
-- is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serifNoItalic
  :: Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serifNoItalic = _boldSplit serifNoBoldNoItalic serifBoldNoItalic

-- | Convert the given character to a mathematical symbol with serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent serif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
serif'
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated with serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
serif' = _italicSplit serifNoItalic' serifItalic'

-- | Convert the given character to a mathematical symbol with serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the charcter is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
serif
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
serif = _italicSplit serifNoItalic serifItalic

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and no /italics/. This maps characters to itself for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifNoBoldNoItalic'
  :: Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated without serifs, not in bold and not in italics.
sansSerifNoBoldNoItalic' = _baseUpperLower 0x1d559

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and no /italics/ wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifNoBoldNoItalic
  :: Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifNoBoldNoItalic = _withCondition isAsciiAlpha sansSerifNoBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/ and in /italics/. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifNoBoldItalic'
  :: Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated without serifs, not in bold and in italics.
sansSerifNoBoldItalic' = _baseUpperLower 0x1d5c1

-- | Convert the given character to a mathematical symbol without serifs, with no
-- /bold/, and in /italics/ wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifNoBoldItalic
  :: Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifNoBoldItalic = _withCondition isAsciiAlpha sansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- not in /italics/. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifBoldNoItalic'
  :: Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated without serifs, in bold and not in italics.
sansSerifBoldNoItalic' = _baseUpperLower 0x1d58d

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and no /italics/ wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifBoldNoItalic
  :: Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifBoldNoItalic = _withCondition isAsciiAlpha sansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, with in
-- /bold/ and in /italics/. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifBoldItalic'
  :: Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated without serifs, in bold and in italics.
sansSerifBoldItalic' = _baseUpperLower 0x1d5f5

-- | Convert the given character to a mathematical symbol without serifs, in
-- /bold/, and in /italics/ wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifBoldItalic
  :: Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifBoldItalic = _withCondition isAsciiAlpha sansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- and in a /italics/ type. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifBold'
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated without serifs, in bold and depending on the given 'ItalicType' in italics or not.
sansSerifBold' = _italicSplit sansSerifBoldNoItalic' sansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in /bold/
-- with the given /italics/ type wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifBold
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifBold = _italicSplit sansSerifBoldNoItalic sansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, not in /bold/
-- and in a /italics/ type. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifNoBold'
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated without serifs, not in bold and depending on the given 'ItalicType' in italics or not.
sansSerifNoBold' = _italicSplit sansSerifNoBoldNoItalic' sansSerifNoBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, with no /bold/
-- and in the given /italics/ type wrapped in a 'Just'. If the charcter is outside the
-- @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifNoBold
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifNoBold = _italicSplit sansSerifNoBoldNoItalic sansSerifNoBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and in italics. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifItalic'
  :: Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated without serifs, depending on the given 'Emphasis' in bold or not, and in italics.
sansSerifItalic' = _boldSplit sansSerifNoBoldItalic' sansSerifBoldItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in /italics/ wrapped in a 'Just'. If the charcter
-- is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifItalic
  :: Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifItalic = _boldSplit sansSerifNoBoldItalic sansSerifBoldItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and not in italics. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerifNoItalic'
  :: Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated without serifs, depending on the given 'Emphasis' in bold or not, and not in italics.
sansSerifNoItalic' = _boldSplit sansSerifNoBoldNoItalic' sansSerifBoldNoItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and not in /italics/ wrapped in a 'Just'. If the charcter
-- is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerifNoItalic
  :: Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerifNoItalic = _boldSplit sansSerifNoBoldNoItalic sansSerifBoldNoItalic

-- | Convert the given character to a mathematical symbol without serifs, with a
-- given /emphasis/ and a given /italics/ style. This maps characters an equivalent sansSerif symbol for the @A@-@Z@ and
-- @a@-@z@ range. For characters outside the range, the behavior is unspecified.
sansSerif'
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Char -- ^ The equivalent character that is formated without serifs, depending on the given 'Emphasis' in bold or not, and depending on the given 'ItalicType' in italics or not.
sansSerif' = _italicSplit sansSerifNoItalic' sansSerifItalic'

-- | Convert the given character to a mathematical symbol without serifs, in the
-- given /emphasis/ and in the given /italics/ type wrapped in a 'Just'. If
-- the charcter is outside the @A@-@Z@ and @a@-@z@ range, 'Nothing' is returned.
sansSerif
  :: ItalicType -- ^ The given 'ItalicType' to use.
  -> Emphasis -- ^ The given 'Emphasis' to use.
  -> Char -- ^ The given character to convert.
  -> Maybe Char -- ^ The equivalent character wrapped in a 'Just' if in the valid range, 'Nothing' otherwise.
sansSerif = _italicSplit sansSerifNoItalic sansSerifItalic

monospace' :: Char -> Char
monospace' = _baseUpperLowerNum 0x1d7c6 0x1d629

monospace :: Char -> Maybe Char
monospace = _withCondition isAsciiAlpha monospace'

scriptRegular' :: Char -> Char
scriptRegular' 'B' = '\x212c'
scriptRegular' 'E' = '\x2130'
scriptRegular' 'F' = '\x2131'
scriptRegular' 'H' = '\x210b'
scriptRegular' 'I' = '\x2110'
scriptRegular' 'L' = '\x2112'
scriptRegular' 'M' = '\x2133'
scriptRegular' 'R' = '\x211b'
scriptRegular' 'e' = '\x212f'
scriptRegular' 'g' = '\x210a'
scriptRegular' 'o' = '\x2134'
scriptRegular' c = _baseUpperLower 0x1d455 c

scriptRegular :: Char -> Maybe Char
scriptRegular = _withCondition isAsciiAlpha scriptRegular'

scriptBold' :: Char -> Char
scriptBold' = _baseUpperLower 0x1d489

scriptBold :: Char -> Maybe Char
scriptBold = _withCondition isAsciiAlpha scriptBold'

script' :: Emphasis -> Char -> Char
script' = _boldSplit scriptRegular' scriptBold'

script :: Emphasis -> Char -> Maybe Char
script = _boldSplit scriptRegular scriptBold


calligraphy' :: Emphasis -> Char -> Char
calligraphy' = script'

calligraphy :: Emphasis -> Char -> Maybe Char
calligraphy = script

calligraphyRegular' :: Char -> Char
calligraphyRegular' = scriptRegular'

calligraphyRegular :: Char -> Maybe Char
calligraphyRegular = scriptRegular

calligraphyBold' :: Char -> Char
calligraphyBold' = scriptBold'

calligraphyBold :: Char -> Maybe Char
calligraphyBold = scriptRegular

-- | Obtain the double struck symbol for the given character. The supported
-- range of characters are the alphabet character (@A@-@Z@, and @a@-@z@), and
-- the numerical characters (@0@-@9@). For characters other than these, the
-- behaviour is unspecified.
doubleStruck'
  :: Char  -- ^ The character to convert to a /double struck/ symbol.
  -> Char -- ^ The double struck symbol for the given character. If the character
          -- is not an ASCII alphanumerical character, the result is
          -- unspecified.
doubleStruck' 'C' = '\x2102'
doubleStruck' 'H' = '\x210d'
doubleStruck' 'N' = '\x2115'
doubleStruck' 'P' = '\x2119'
doubleStruck' 'Q' = '\x211a'
doubleStruck' 'R' = '\x211d'
doubleStruck' 'Z' = '\x2124'
doubleStruck' c = _baseUpperLowerNum 0x1d7a8 0x1d4f1 c

-- | Obtain the double struck symbol for the given character. The supported
-- range of characters are the alphabet characters (@A@-@Z@, and @a@-@z@), and
-- the numerical characters (@0@-@9@). The symbols are wrapped in the 'Just'
-- data constructor. For characters outside the range, 'Nothing' is returned.
doubleStruck
  :: Char -- ^ The character to convert to a /double struck/ symbol.
  -> Maybe Char -- ^ The double struck symbol for the given character wrapped
                -- in a 'Just' data constructor, 'Nothing' if there is no
                -- equivalent /double stuck/ character.
doubleStruck = _withCondition isAsciiAlphaNum doubleStruck'

-- | Obtain the fraktur symbol for the given character in a regular (not /bold/)
-- style. The supported range of characters are the alphabet characters
-- (@A@-@Z@, and @a@-@z@). In case the character is not in this range, it is
-- unspecified what will be returned.
frakturRegular'
  :: Char -- ^ The character to convert to a regular /fraktur/ symbol.
  -> Char -- ^ The equivalent /fraktur/ charater for the given character.
frakturRegular' 'C' = '\x212d'
frakturRegular' 'H' = '\x210c'
frakturRegular' 'I' = '\x2111'
frakturRegular' 'R' = '\x211c'
frakturRegular' 'Z' = '\x2128'
frakturRegular' c = _baseUpperLower 0x1d4bd c

-- | Obtain the fraktur symbol for the given character in a regular (not /bold/
-- style). The result is wrapped in a 'Just' data constructor. The range of
-- supported characters are the alphabet characters (@A@-@Z@, and @a@-@z@).
-- In case a character outside the range is passed to the function, 'Nothing' is
-- returned.
frakturRegular
  :: Char -- ^ The character to convert to a regular /fraktur/ symbol.
  -> Maybe Char -- ^ The fraktur symbol for the given character wrapped
                -- in a 'Just' data constructor, 'Nothing' if there is no
                -- equivalent /fraktur/ character.
frakturRegular = _withCondition isAsciiAlpha frakturRegular'

-- | Obtain the fraktur symbol for the given character in a /bold/
-- style. The supported range of characters are the alphabet characters
-- (@A@-@Z@, and @a@-@z@). In case the character is not in this range, it is
-- unspecified what will be returned.
frakturBold'
  :: Char -- ^ The character to convert to a bold /fraktur/ symbol.
  -> Char -- ^ The equivalent /fraktur/ charater for the given character.
frakturBold' = _baseUpperLower 0x1d525

-- | Obtain the fraktur symbol for the given character in a /bold/.
-- The result is wrapped in a 'Just' data constructor. The range of
-- supported characters are the alphabet characters (@A@-@Z@, and @a@-@z@).
-- In case a character outside the range is passed to the function, 'Nothing' is
-- returned.
frakturBold
  :: Char -- ^ The character to convert to a bold /fraktur/ symbol.
  -> Maybe Char -- ^ The fraktur symbol for the given character wrapped
                -- in a 'Just' data constructor, 'Nothing' if there is no
                -- equivalent /fraktur/ character.
frakturBold = _withCondition isAsciiAlpha frakturBold'

-- | Obtain the fraktur symbol for the given character in the given emphasis
-- style. The supported range of characters are the alphabet characters
-- (@A@-@Z@, and @a@-@z@). In case the character is not in this range, it is
-- unspecified what will be returned.
fraktur'
  :: Emphasis -- ^ The given emphasis style to use.
  -> Char -- ^ The character to convert to a /fraktur/ symbol in the given emphasis style.
  -> Char -- ^ The equivalent /fraktur/ charater for the given character.
fraktur' = _boldSplit frakturRegular' frakturBold'

-- | Obtain the fraktur symbol for the given character in the given emphais
-- style. The result is wrapped in a 'Just' data constructor. The range of
-- supported characters are the alphabet characters (@A@-@Z@, and @a@-@z@).
-- In case a character outside the range is passed to the function, 'Nothing' is
-- returned.
fraktur
  :: Emphasis -- ^ The given emphasis style to use.
  -> Char -- ^ The character to convert to a /fraktur/ symbol with the given emphasis style.
  -> Maybe Char -- ^ The fraktur symbol for the given character wrapped
                -- in a 'Just' data constructor, 'Nothing' if there is no
                -- equivalent /fraktur/ character.
fraktur = _boldSplit frakturRegular frakturBold

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.Char.Core
-- Description : A module that defines data structures used in the other modules.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module defines data structures that are used in other modules, for example to rotate the characters.
module Data.Char.Core
  ( -- * Possible rotations
    Orientation (Horizontal, Vertical),
    Rotate90 (R0, R90, R180, R270),

    -- * Rotated objects
    Oriented (Oriented, oobject, orientation),
    Rotated (Rotated, robject, rotation),

    -- * Letter case
    LetterCase (UpperCase, LowerCase),
    splitLetterCase,

    -- * Ligating
    Ligate (Ligate, NoLigate),
    splitLigate,
    ligate,
    ligateF,

    -- * Types of fonts
    Emphasis (NoBold, Bold),
    splitEmphasis,
    ItalicType (NoItalic, Italic),
    splitItalicType,
    FontStyle (SansSerif, Serif),
    splitFontStyle,

    -- * Character range checks
    isAsciiAlphaNum,
    isAsciiAlpha,
    isGreek,
    isACharacter,
    isNotACharacter,
    isReserved,
    isNotReserved,

    -- * Map characters from and to 'Enum's
    mapFromEnum,
    mapToEnum,
    mapToEnumSafe,
    liftNumberFrom,
    liftNumberFrom',
    liftNumber,
    liftNumber',
    liftDigit,
    liftDigit',
    liftUppercase,
    liftUppercase',
    liftLowercase,
    liftLowercase',
    liftUpperLowercase,
    liftUpperLowercase',

    -- * Convert objects from and to Unicode 'Char'acters
    UnicodeCharacter (toUnicodeChar, fromUnicodeChar, fromUnicodeChar', isInCharRange),
    UnicodeChar,
    UnicodeText (toUnicodeText, fromUnicodeText, fromUnicodeText', isInTextRange),
    generateIsInTextRange,
    generateIsInTextRange',

    -- * Mirroring items horizontally and/or vertically
    MirrorHorizontal (mirrorHorizontal),
    MirrorVertical (mirrorVertical),

    -- * Ways to display numbers
    PlusStyle (WithoutPlus, WithPlus),
    splitPlusStyle,

    -- * Functions to implement a number system
    withSign,
    signValueSystem,
    positionalNumberSystem,
    positionalNumberSystem10,

    -- * Re-export of some functions of the 'Data.Char' module
    chr,
    isAlpha,
    isAlphaNum,
    isAscii,
    ord,
  )
where

import Control.DeepSeq (NFData, NFData1)
import Data.Bits ((.&.))
import Data.Char (chr, isAlpha, isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, ord)
import Data.Data (Data)
import Data.Default.Class (Default (def))
import Data.Functor.Classes (Eq1 (liftEq), Ord1 (liftCompare))
import Data.Hashable (Hashable)
import Data.Hashable.Lifted (Hashable1)
import Data.Maybe (fromJust, isJust)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif
import Data.Text (Text, cons, null, pack, singleton, snoc, uncons, unpack)
import GHC.Generics (Generic, Generic1)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), Arbitrary1 (liftArbitrary), arbitrary1, arbitraryBoundedEnum)
import Prelude hiding (null)

-- | Specify whether we write a value in 'UpperCase' or 'LowerCase'. The
-- 'Default' is 'UpperCase', since for example often Roman numerals are written
-- in /upper case/.
data LetterCase
  = -- | The /upper case/ formatting.
    UpperCase
  | -- | The /lower case/ formatting.
    LowerCase
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable LetterCase

instance NFData LetterCase

-- | Pick one of the two values based on the 'LetterCase' value.
splitLetterCase ::
  -- | The value to return in case of 'UpperCase'.
  a ->
  -- | The value to return in case of 'LowerCase'.
  a ->
  -- | The given /letter case/.
  LetterCase ->
  -- | One of the two given values, depending on the 'LetterCase' value.
  a
splitLetterCase x y = go
  where
    go UpperCase = x
    go LowerCase = y

-- | Specify whether we write a positive number /with/ or /without/ a plus sign.
-- the 'Default' is 'WithoutPlus'.
data PlusStyle
  = -- | Write positive numbers /without/ using a plus sign.
    WithoutPlus
  | -- | Write positive numbers /with/ a plus sign.
    WithPlus
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable PlusStyle

instance NFData PlusStyle

-- | Pick one of the two values based on the 't:PlusStyle' value.
splitPlusStyle ::
  -- | The value to return in case of 'WithoutPlus'.
  a ->
  -- | The value to return in case of 'WithPlus'.
  a ->
  -- | The plus style.
  PlusStyle ->
  -- | One of the two given values, based on the 't:PlusStyle' value.
  a
splitPlusStyle x y = go
  where
    go WithoutPlus = x
    go WithPlus = y

-- | The possible orientations of a unicode character, these can be
-- /horizontal/, or /vertical/.
data Orientation
  = -- | /Horizontal/ orientation.
    Horizontal
  | -- | /Vertical/ orientation.
    Vertical
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable Orientation

instance NFData Orientation

-- | A data type that specifies that an item has been given an orientation.
data Oriented a = Oriented
  { -- | The object that is oriented.
    oobject :: a,
    -- | The oriented of the oriented object.
    orientation :: Orientation
  }
  deriving (Bounded, Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance Eq1 Oriented where
  liftEq cmp ~(Oriented ba oa) ~(Oriented bb ob) = cmp ba bb && oa == ob

instance Hashable1 Oriented

instance Hashable a => Hashable (Oriented a)

instance NFData a => NFData (Oriented a)

instance NFData1 Oriented

instance Ord1 Oriented where
  liftCompare cmp ~(Oriented ba oa) ~(Oriented bb ob) = cmp ba bb <> compare oa ob

-- | Possible rotations of a unicode character if that character can be rotated
-- over 0, 90, 180, and 270 degrees.
data Rotate90
  = -- | No rotation.
    R0
  | -- | Rotation over /90/ degrees.
    R90
  | -- | Rotation over /180/ degrees.
    R180
  | -- | Rotation over /270/ degrees.
    R270
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable Rotate90

instance NFData Rotate90

-- | A data type that specifies that an item has been given a rotation.
data Rotated a = Rotated
  { -- | The object that is rotated.
    robject :: a,
    -- | The rotation of the rotated object.
    rotation :: Rotate90
  }
  deriving (Bounded, Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance Eq1 Rotated where
  liftEq cmp ~(Rotated oa ra) ~(Rotated ob rb) = cmp oa ob && ra == rb

instance Hashable1 Rotated

instance Hashable a => Hashable (Rotated a)

instance NFData a => NFData (Rotated a)

instance NFData1 Rotated

instance Ord1 Rotated where
  liftCompare cmp ~(Rotated oa ra) ~(Rotated ob rb) = cmp oa ob <> compare ra rb

-- | A data type that lists the possible emphasis of a font. This can be 'Bold'
-- or 'NoBold' the 'Default' is 'NoBold'.
data Emphasis
  = -- | The characters are not stressed with boldface.
    NoBold
  | -- | The characters are stressed in boldface.
    Bold
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable Emphasis

instance NFData Emphasis

-- | Pick one of the two values based on the 't:Emphasis' value.
splitEmphasis ::
  -- | The value to return in case of 'NoBold'.
  a ->
  -- | The value to return in case of 'Bold'.
  a ->
  -- | The emphasis type.
  Emphasis ->
  -- | One of the two given values, based on the 't:Emphasis' value.
  a
splitEmphasis x y = go
  where
    go NoBold = x
    go Bold = y

-- | A data type that can be used to specify if an /italic/ character is used.
-- The 'Default' is 'NoItalic'.
data ItalicType
  = -- | No italic characters are used.
    NoItalic
  | -- | Italic characters are used.
    Italic
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable ItalicType

instance NFData ItalicType

-- | Pick one of the two values based on the 't:ItalicType' value.
splitItalicType ::
  -- | The value to return in case of 'NoItalic'.
  a ->
  -- | The value to return in case of 'Italic'.
  a ->
  -- | The italic type.
  ItalicType ->
  -- | One of the two given values, based on the 't:ItalicType' value.
  a
splitItalicType x y = go
  where
    go NoItalic = x
    go Italic = y

-- | A data type that specifies if the font is with /serifs/ or not. The
-- 'Defaul;t' is 'Serif'.
data FontStyle
  = -- | The character is a character rendered /without/ serifs.
    SansSerif
  | -- | The character is a character rendered /with/ serifs.
    Serif
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable FontStyle

instance NFData FontStyle

-- | Pick one of the two values based on the 't:FontStyle' value.
splitFontStyle ::
  -- | The value to return in case of 'SansSerif'.
  a ->
  -- | The value to return in case of 'Serif'.
  a ->
  -- | The font style.
  FontStyle ->
  -- | One of the two given values, based on the 't:FontStyle' value.
  a
splitFontStyle x y = go
  where
    go SansSerif = x
    go Serif = y

-- | Specify if one should ligate, or not. When litigation is done
-- characters that are normally written in two (or more) characters
-- are combined in one character. For example @Ⅲ@ instead of @ⅠⅠⅠ@.
data Ligate
  = -- | A ligate operation is performed on the characters, the 'def' for 't:Ligate'.
    Ligate
  | -- | No ligate operation is performed on the charaters.
    NoLigate
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable Ligate

instance NFData Ligate

-- | Pick one of the two values based on the value for 't:Ligate'.
splitLigate ::
  -- | The value to return in case of 'v:Ligate'.
  a ->
  -- | The value to return in case of 'NoLigate'.
  a ->
  -- | The ligation style.
  Ligate ->
  -- | One of the two given values, based on the 't:Ligate' value.
  a
splitLigate x y = go
  where
    go Ligate = x
    go NoLigate = y

-- | Specify if the given ligate function should be performed on the input,
-- if 'v:Ligate' is passed, and the /identity/ function otherwise.
ligate :: (a -> a) -> Ligate -> a -> a
ligate f Ligate = f
ligate _ NoLigate = id

-- | Specify if the given ligate function is performed over the functor object
-- if 'v:Ligate' is passed, and the /identity/ function otherwise.
ligateF :: Functor f => (a -> a) -> Ligate -> f a -> f a
ligateF = ligate . fmap

-- | Checks if a charcter is an /alphabetic/ character in ASCII. The characters
-- @"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"@ satisfy this
-- predicate.
isAsciiAlpha :: Char -> Bool
isAsciiAlpha x = isAscii x && isAlpha x

-- | Checks if a character is an /alphabetic/ or /numerical/ character in ASCII.
-- The characters @0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz@
-- satisfy this predicate.
isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum x = isAscii x && isAlphaNum x

-- | Checks if a character is a basic /greek alphabetic/ character or a Greek-like symbol.
-- The characters @ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ∇ϴαβγδεζηθικλμνξοπρςστυφχψω∂ϵϑϰϕϱϖ@ satisfy this predicate.
isGreek :: Char -> Bool
isGreek 'ϑ' = True -- U+03D1 GREEK THETA SYMBOL
isGreek 'ϕ' = True -- U+03D5 GREEK PHI SYMBOL
isGreek 'ϖ' = True -- U+03D6 GREEK PI SYMBOL
isGreek 'ϰ' = True -- U+03F0 GREEK KAPPA SYMBOL
isGreek 'ϱ' = True -- U+03F1 GREEK RHO SYMBOL
isGreek 'ϴ' = True -- U+03F4 GREEK CAPITAL THETA SYMBOL
isGreek 'ϵ' = True -- U+03F5 GREEK LUNATE EPSILON SYMBOL
isGreek '∂' = True -- U+2202 PARTIAL DIFFERENTIAL
isGreek '∇' = True -- U+2207 NABLA
isGreek c =
  ('Α' <= c && c <= 'Ω' && c /= '\x03A2') -- U+0391 GREEK CAPITAL LETTER ALPHA, U+03A9 GREEK CAPITAL LETTER OMEGA
    || ('α' <= c && c <= 'ω') -- U+03B1 GREEK SMALL LETTER ALPHA, U+03C9 GREEK SMALL LETTER OMEGA

-- | Calculate for a given plus and minus sign a 'Text' object for the given
-- number in the given 'PlusStyle'.
withSign ::
  Integral i =>
  -- | The function that maps the absolute value of the number to a 'Text' object that is appended to the sign.
  (i -> Text) ->
  -- | The /plus/ sign to use.
  Char ->
  -- | The /minus/ sign to use.
  Char ->
  -- | The given 'PlusStyle' to use.
  PlusStyle ->
  -- | The given 'Integral' number to render.
  i ->
  -- | A 'Text' object that represents the given number, with the given sign numbers in the given 'PlusStyle'.
  Text
withSign f cp cn ps n
  | n < 0 = cons cn (f (-n))
  | WithPlus <- ps = cons cp (f n)
  | otherwise = f n

-- | A function to make it more convenient to implement a /sign-value system/.
-- This is done for a given /radix/ a function that maps the given value and the
-- given weight to a 'Text' object, a 'Text' object for /zero/ (since in some
-- systems that is different), and characters for /plus/ and /minus/.
-- The function then will for a given 'PlusStyle' convert the number to a
-- sequence of characters with respect to how the /sign-value system/ is
-- implemented.
signValueSystem ::
  Integral i =>
  -- | The given /radix/ to use.
  i ->
  -- | A function that maps the /value/ and the /weight/ to a 'Text' object.
  (Int -> Int -> Text) ->
  -- | The given 'Text' used to represent /zero/.
  Text ->
  -- | The given 'Char' used to denote /plus/.
  Char ->
  -- | The given 'Char' used to denote /minus/.
  Char ->
  -- | The given 'PlusStyle' to use.
  PlusStyle ->
  -- | The given number to convert.
  i ->
  -- | A 'Text' object that denotes the given number with the given /sign-value system/.
  Text
signValueSystem radix fi zero = withSign (f 0)
  where
    f 0 0 = zero
    f i n
      | n < radix = fi' n i
      | otherwise = f (i + 1) q <> fi' r i
      where
        (q, r) = quotRem n radix
    fi' = flip fi . fromIntegral

-- | A function to make it more convenient to implement a /positional number
-- system/. This is done for a given /radix/ a given conversion funtion that
-- maps a value to a 'Char', and a 'Char' for /plus/ and /minus/.
-- The function then construct a 'Text' object for a given 'PlusStyle' and a given number.
positionalNumberSystem ::
  Integral i =>
  -- | The given radix to use.
  i ->
  -- | A function that maps the value of a /digit/ to the corresponding 'Char'.
  (Int -> Char) ->
  -- | The given character used to denote /plus/.
  Char ->
  -- | The given character used to denote /minus/.
  Char ->
  -- | The given 'PlusStyle' to use.
  PlusStyle ->
  -- | The given number to convert.
  i ->
  -- | A 'Text' object that denotes the given number with the given /positional number system/.
  Text
positionalNumberSystem radix fi = withSign f
  where
    f n
      | n < radix = singleton (fi' n)
      | otherwise = snoc (f q) (fi' r)
      where
        (q, r) = quotRem n radix
    fi' = fi . fromIntegral

-- | A function to make it more convenient to implement a /positional number
-- system/ with /radix/ 10.
positionalNumberSystem10 ::
  Integral i =>
  -- | A function that maps the value of a /digit/ to the corresponding 'Char'.
  (Int -> Char) ->
  -- | The given character used to denote /plus/.
  Char ->
  -- | The given character used to denote /minus/.
  Char ->
  -- | The given 'PlusStyle' to use.
  PlusStyle ->
  -- | The given number to convert.
  i ->
  -- | A 'Text' object that denotes the given number with the given /positional number system/.
  Text
positionalNumberSystem10 = positionalNumberSystem 10

-- | Check if the given character is not a /reserved character/. This is denoted in
-- the Unicode documentation with @\<reserved\>@.
isNotReserved ::
  -- | The given 'Char'acter to check.
  Char ->
  -- | 'True' if the given 'Char'acter is not reserved; 'False' otherwise.
  Bool
isNotReserved = not . isReserved

-- | Check if the given character is a /reserved character/. This is denoted in
-- the Unicode documentation with @\<reserved\>@.
isReserved ::
  -- | The given 'Char'acter to check.
  Char ->
  -- | 'True' if the given 'Char'acter is reserved; 'False' otherwise.
  Bool
isReserved '\x9e4' = True
isReserved '\x9e5' = True
isReserved '\xa64' = True
isReserved '\xa65' = True
isReserved '\xae4' = True
isReserved '\xae5' = True
isReserved '\xb64' = True
isReserved '\xb65' = True
isReserved '\xbe4' = True
isReserved '\xbe5' = True
isReserved '\xc64' = True
isReserved '\xc65' = True
isReserved '\xce4' = True
isReserved '\xce5' = True
isReserved '\xd64' = True
isReserved '\xd65' = True
isReserved '\x2072' = True
isReserved '\x2073' = True
isReserved '\x1d4a0' = True
isReserved '\x1d4a1' = True
isReserved '\x1d4a3' = True
isReserved '\x1d4a4' = True
isReserved '\x1d4a7' = True
isReserved '\x1d4a8' = True
isReserved '\x1d50b' = True
isReserved '\x1d50c' = True
isReserved '\x1d455' = True
isReserved '\x1d49d' = True
isReserved '\x1d4ad' = True
isReserved '\x1d4ba' = True
isReserved '\x1d4bc' = True
isReserved '\x1d4c4' = True
isReserved '\x1d506' = True
isReserved '\x1d515' = True
isReserved '\x1d51d' = True
isReserved '\x1d53a' = True
isReserved '\x1d53f' = True
isReserved '\x1d545' = True
isReserved '\x1d551' = True
isReserved c = '\x1d547' <= c && c <= '\x1d549'

-- | Check if the given character is a character according to the Unicode
-- specifications. Codepoints that are not a character are denoted in the
-- Unicode documentation with @\<not a character\>@.
isACharacter ::
  -- | The given 'Char'acter to check.
  Char ->
  -- | 'True' if the given 'Char'acter is a character (according to the Unicode specifications); 'False' otherwise.
  Bool
isACharacter c = ord c .&. 0xfffe /= 0xfffe && ('\xfdd0' > c || c > '\xfdef')

-- | Check if the given character is not a character according to the Unicode
-- specifications. The Unicode documentation denotes these with @\<not a character\>@.
isNotACharacter ::
  -- | The given 'Char'acter to check.
  Char ->
  -- | 'True' if the given 'Char'acter is not a character (according to the Unicode specifications); 'False' otherwise.
  Bool
isNotACharacter c = ord c .&. 0xfffe == 0xfffe || '\xfdd0' <= c && c <= '\xfdef'

-- | Map the given 'Char' object to an object with a type that is an instance of
-- 'Enum' with a given offset for the 'Char'acter range.
mapToEnum ::
  Enum a =>
  -- | The given /offset/ value.
  Int ->
  -- | The 'Char'acter to map to an 'Enum' object.
  Char ->
  -- | The given 'Enum' object for the given 'Char'.
  a
mapToEnum o = toEnum . subtract o . ord

-- | Map the given 'Char' object to an object with a type that is an instance of
-- 'Enum'. It first checks if the mapping results in a value between the
-- 'fromEnum' values for 'minBound' and 'maxBound'.
mapToEnumSafe ::
  forall a.
  (Bounded a, Enum a) =>
  -- | The given /offset/ value.
  Int ->
  -- | The given 'Char'acter to map to an 'Enum' object.
  Char ->
  -- | The given 'Enum' object for the given 'Char'acter wrapped in a 'Just' if that exists; 'Nothing' otherwise.
  Maybe a
mapToEnumSafe o = go
  where
    go c
      | e0 <= ei && ei <= en = Just (toEnum ei)
      | otherwise = Nothing
      where
        ei = ord c - o
    e0 = fromEnum (minBound :: a)
    en = fromEnum (maxBound :: a)

-- | Map the given object with a type that is an instance of 'Enum' to a
-- 'Char'acter with a given offset for the 'Char'acter value.
mapFromEnum ::
  Enum a =>
  -- | The given /offset/ value.
  Int ->
  -- | The given 'Enum' value to convert to a 'Char'acter.
  a ->
  -- | The character that corresponds to the given 'Enum' object.
  Char
mapFromEnum o = chr . (o +) . fromEnum

-- | An alias of the 'UnicodeCharacter' type class.
type UnicodeChar = UnicodeCharacter

-- | A class from which objects can be derived that map to and from a /single/
-- unicode character.
class UnicodeCharacter a where
  -- | Convert the given object to a Unicode 'Char'acter.
  toUnicodeChar ::
    -- | The given object to convert to a 'Char'acter.
    a ->
    -- | The equivalent Unicode 'Char'acter.
    Char

  -- | Convert the given 'Char'acter to an object wrapped in a 'Just' data
  -- constructor if that exists; 'Nothing' otherwise.
  fromUnicodeChar ::
    -- | The given 'Char'acter to convert to an element.
    Char ->
    -- | An element if the given 'Char'acter maps to an element wrapped in a 'Just'; 'Nothing' otherwise.
    Maybe a

  -- | Convert the given 'Char'acter to an object. If the 'Char'acter does not
  -- map on an element, the behavior is /unspecified/, it can for example
  -- result in an error.
  fromUnicodeChar' ::
    -- | The given 'Char'acter to convert to an element.
    Char ->
    -- | The given element that is equivalent to the given 'Char'acter.
    a
  fromUnicodeChar' = fromJust . fromUnicodeChar

  -- | Check if the given 'Char'acter maps on an item of @a@.
  isInCharRange ::
    -- | The given 'Char'acter to test.
    Char ->
    -- | 'True' if the given 'Char'acter has a corresponding value for @a@; 'False' otherwise.
    Bool
  isInCharRange = isJust . (fromUnicodeChar @a)

  {-# MINIMAL toUnicodeChar, fromUnicodeChar #-}

-- | A class from which boejcts can be derived that map to and from a /sequence/
-- of unicode characters.
class UnicodeText a where
  -- | Convert the given object to a 'Text' object.
  toUnicodeText ::
    -- | The given object to convert to a 'Text' object.
    a ->
    -- | A 'Text' object that is the Unicode representation of the element.
    Text
  default toUnicodeText :: UnicodeCharacter a => a -> Text
  toUnicodeText = singleton . toUnicodeChar

  -- | Convert the given 'Text' to an object wrapped in a 'Just' data
  -- constructor if that exists; 'Nothing' otherwise.
  fromUnicodeText ::
    -- | The given 'Text' to convert to an object.
    Text ->
    -- | The equivalent object wrapped in a 'Just' data constructor if it exists; 'Nothing' otherwise.
    Maybe a
  default fromUnicodeText :: UnicodeCharacter a => Text -> Maybe a
  fromUnicodeText t
    | [c] <- unpack t = fromUnicodeChar c
    | otherwise = Nothing

  -- | Convert the given 'Text' to an object. If the 'Text' does not map on
  -- an element, the behavior is /unspecified/, it can for example result in
  -- an error.
  fromUnicodeText' ::
    -- | The given 'Text' to convert to an object.
    Text ->
    -- | The given equivalent object. If there is no equivalent object, the behavior is unspecified.
    a
  fromUnicodeText' = fromJust . fromUnicodeText

  -- | Determine if the given 'Text' value maps on a value of type @a@.
  isInTextRange ::
    -- | The given 'Text' object to test.
    Text ->
    -- | 'True' if there is a counterpart of type @a@; 'False' otherwise.
    Bool
  isInTextRange = isJust . (fromUnicodeText @a)

-- | Convert a given 'isInCharRange' check into a 'isInTextRange' check.
generateIsInTextRange ::
  -- | The given 'isInCharRange' check.
  (Char -> Bool) ->
  -- | The 'Text' object to check.
  Text ->
  -- | 'True' if the given 'Text' object has a single character for which the 'isInCharRange' check succeeds, 'False' otherwise.
  Bool
generateIsInTextRange f = go . uncons
  where
    go (Just (c, t)) = null t && f c
    go Nothing = False

-- | Generate an 'isInTextRange' check with the 'isInCharRange' check for the instance of 'UnicodeCharacter' of that type.
generateIsInTextRange' ::
  forall a.
  UnicodeCharacter a =>
  -- | The given 'Text' object to check.
  Text ->
  -- | 'True' if the given 'Text' object has a single character for which the 'isInCharRange' check succeeds, 'False' otherwise.
  Bool
generateIsInTextRange' = generateIsInTextRange (isInCharRange @a)

-- | A type class that specifies that the items can be mirrored in the /horizontal/ direction (such that up is now down).
-- The mirror is /not/ per se /pixel perfect/. For example the mirror of 🂁 is 🁵, so the dots of the bottom pat
-- of the domino are not mirrored correctly.
class MirrorHorizontal a where
  -- | Obtain the /horizontally/ mirrored variant of the given item. Applying the same function twice should
  -- return the original object.
  mirrorHorizontal ::
    -- | The given item to mirror /horizontally/.
    a ->
    -- | The corresponding mirrored item.
    a

  {-# MINIMAL mirrorHorizontal #-}

-- | A type class that specifies that the items can be mirrored in the /vertical/ direction (such that left is now right).
-- The mirror is /not/ per se pixel perfect. For example the vertical mirror of 🁏 is 🁃, so the dots of the right part
-- of the domino are not mirrored correctly.
class MirrorVertical a where
  -- | Obtain the /vertically/ mirrored variant of the given item. Applying the same function twice should
  -- return the original object.
  mirrorVertical ::
    -- | The given item to mirror /vertically/.
    a ->
    -- | The corresponding mirrored item.
    a

  {-# MINIMAL mirrorVertical #-}

-- | Construct a function that maps digits to the character with the given value
-- for the offset.
liftNumberFrom ::
  -- | The given offset value.
  Int ->
  -- | The maximum value that can be mapped.
  Int ->
  -- | The given Unicode value used for the offset.
  Int ->
  -- | The given number to convert, must be between the offset and the maximum.
  Int ->
  -- | The corresponding 'Char'acter wrapped in a 'Just' if the number is between the offset and the maximum; 'Nothing' otherwise.
  Maybe Char
liftNumberFrom o m d = go
  where
    go n
      | n >= o && n <= m = Just (chr (d' + n))
      | otherwise = Nothing
    !d' = d - o

-- | Construct a function that maps digits to the character with the given value
-- for the offset.
liftNumberFrom' ::
  -- | The given offset value.
  Int ->
  -- | The given Unicode value used for the offset.
  Int ->
  -- | The given number to convert to a corresponding 'Char'acter.
  Int ->
  -- | The corresponding 'Char'acter for the given mapping function.
  Char
liftNumberFrom' o d = chr . (d' +)
  where
    !d' = d - o

-- | Construct a function that maps digits to the character with the given value
-- for @0@.
liftNumber ::
  -- | The maximum value that can be mapped.
  Int ->
  -- | The given Unicode value used for @0@.
  Int ->
  -- | The given digit to convert to a number between 0 and the maximum.
  Int ->
  -- | The corresponding 'Char'acter wrapped in a 'Just' if the number is between @0@ and @9@; 'Nothing' otherwise.
  Maybe Char
liftNumber = liftNumberFrom 0

-- | Construct a function that maps digits to characters with the given value
-- for @0@.
liftNumber' ::
  -- | The  given Unicode value used for @0@.
  Int ->
  -- | The given digit to convert.
  Int ->
  -- | The corresponding 'Char'acter, for numbers outside the @0-9@ range, the result is unspecified.
  Char
liftNumber' = liftDigit'

-- | Construct a function that maps digits to the character with the given value
-- for @0@.
liftDigit ::
  -- | The given Unicode value used for @0@.
  Int ->
  -- | The given digit to convert to a number between 0 and 9.
  Int ->
  -- | The corresponding 'Char'acter wrapped in a 'Just' if the number is between @0@ and @9@; 'Nothing' otherwise.
  Maybe Char
liftDigit = liftNumber 9

-- | Construct a function that maps digits to characters with the given value
-- for @0@.
liftDigit' ::
  -- | The  given Unicode value used for @0@.
  Int ->
  -- | The given digit to convert, must be between @0@ and @9@.
  Int ->
  -- | The corresponding 'Char'acter, for numbers outside the @0-9@ range, the result is unspecified.
  Char
liftDigit' d = chr . (d +)

-- | Construct a function that maps upper case alphabetic characters with the
-- given value for @A@.
liftUppercase ::
  -- | The given Unicode value for @A@.
  Int ->
  -- | The given character to convert.
  Char ->
  -- | The corresponding character wrapped in a 'Just' if the given character is in the @A-Z@ range; 'Nothing' otherwise.
  Maybe Char
liftUppercase d = go
  where
    go c
      | isAsciiUpper c = Just (chr (d' + ord c))
      | otherwise = Nothing
    !d' = d - 65

-- | Construct a function that maps upper case alphabetic characters with the
-- given value for @A@.
liftUppercase' ::
  -- | The given Unicode value for @A@.
  Int ->
  -- | The given upper case alphabetic value to convert.
  Char ->
  -- | The corresponding character, if the given value is outside the @A-Z@ range, the result is unspecified.
  Char
liftUppercase' d = chr . (d' +) . ord
  where
    !d' = d - 65

-- | Construct a function that maps lower case alphabetic characters with the
-- given value for @a@.
liftLowercase ::
  -- | The given Unicode value for @a@.
  Int ->
  -- | The given character to convert.
  Char ->
  -- | The corresponding character wrapped in a 'Just' if the given character is in the @a-z@ range; 'Nothing' otherwise.
  Maybe Char
liftLowercase d = go
  where
    go c
      | isAsciiLower c = Just (chr (d' + ord c))
      | otherwise = Nothing
    !d' = d - 97

-- | Construct a function that maps lower case alphabetic characters with the
-- given value for @a@.
liftLowercase' ::
  -- | The given Unicode value for @a@.
  Int ->
  -- | The given upper case alphabetic value to convert.
  Char ->
  -- | The corresponding character, if the given value is outside the @a-z@ range, the result is unspecified.
  Char
liftLowercase' d = chr . (d' +) . ord
  where
    !d' = d - 97

-- | Construct a function that maps lower case alphabetic characters with the
-- given values for @A@ and @a@.
liftUpperLowercase ::
  -- | The given Unicode value for @A@.
  Int ->
  -- | The given Unicode value for @a@.
  Int ->
  -- | The given character to convert.
  Char ->
  -- | The corresponding character wrapped in a 'Just' if the given character is in the @A-Z,a-z@ range; 'Nothing' otherwise.
  Maybe Char
liftUpperLowercase du dl = go
  where
    go c
      | isAsciiLower c = Just (chr (dl' + c'))
      | isAsciiUpper c = Just (chr (du' + c'))
      | otherwise = Nothing
      where
        c' = ord c
    !du' = du - 65
    !dl' = dl - 97

-- | Construct a function that maps lower case alphabetic characters with the
-- given values for @A@ and @a@.
liftUpperLowercase' ::
  -- | The given Unicode value for @A@.
  Int ->
  -- | The given Unicode value for @a@.
  Int ->
  -- | The given character to convert.
  Char ->
  -- | The corresponding character if the given character is in the @A-Z,a-z@ range; unspecified otherwise.
  Char
liftUpperLowercase' du dl = go
  where
    go c
      | isAsciiUpper c = chr (du' + c')
      | otherwise = chr (dl' + c')
      where
        c' = ord c
    du' = du - 65
    dl' = dl - 97

instance Arbitrary LetterCase where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Orientation where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (Oriented a) where
  arbitrary = arbitrary1

instance Arbitrary a => Arbitrary (Rotated a) where
  arbitrary = arbitrary1

instance Arbitrary1 Oriented where
  liftArbitrary arb = Oriented <$> arb <*> arbitrary

instance Arbitrary1 Rotated where
  liftArbitrary arb = Rotated <$> arb <*> arbitrary

instance Arbitrary PlusStyle where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Rotate90 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Ligate where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Emphasis where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ItalicType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FontStyle where
  arbitrary = arbitraryBoundedEnum

instance Default LetterCase where
  def = UpperCase

instance Default PlusStyle where
  def = WithoutPlus

instance Default Ligate where
  def = Ligate

instance Default Emphasis where
  def = NoBold

instance Default ItalicType where
  def = NoItalic

instance Default FontStyle where
  def = Serif

instance UnicodeCharacter Char where
  toUnicodeChar = id
  fromUnicodeChar = Just
  fromUnicodeChar' = id
  isInCharRange = const True

instance UnicodeText [Char] where
  toUnicodeText = pack
  fromUnicodeText = Just . unpack
  fromUnicodeText' = unpack
  isInTextRange = const True

instance UnicodeText Char where
  isInTextRange cs
    | Just (_, c) <- uncons cs = null c
    | otherwise = False

instance UnicodeText Text where
  toUnicodeText = id
  fromUnicodeText = Just
  fromUnicodeText' = id
  isInTextRange = const True

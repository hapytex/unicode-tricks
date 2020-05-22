{-# LANGUAGE DeriveTraversable, Safe #-}

{-|
Module      : Data.Char.Core
Description : A module that defines data structures used in the other modules.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module defines data structures that are used in other modules, for example to rotate the characters.
-}

module Data.Char.Core (
    -- * Possible rotations
    Orientation(Horizontal, Vertical)
  , Rotate90(R0, R90, R180, R270)
    -- * Rotated objects
  , Oriented(Oriented, oobject, orientation)
    -- * Letter case
  , LetterCase(UpperCase, LowerCase), splitLetterCase
    -- * Ligating
  , Ligate(Ligate, NoLigate), splitLigate, ligate, ligateF
    -- * Types of fonts
  , Emphasis(NoBold, Bold), splitEmphasis
  , ItalicType(NoItalic, Italic), splitItalicType
  , FontStyle(SansSerif, Serif), splitFontStyle
    -- * Character range checks
  , isAsciiAlphaNum, isAsciiAlpha
    -- * Ways to display numbers
  , PlusStyle(WithoutPlus, WithPlus), splitPlusStyle
    -- * Functions to implement a number system
  , withSign, signValueSystem, positionalNumberSystem, positionalNumberSystem10
    -- * Re-export of some functions of the 'Data.Char' module
  , chr, isAlpha, isAlphaNum, isAscii, ord
  ) where

import Data.Char(chr, isAlpha, isAlphaNum, isAscii, ord)
import Data.Default(Default(def))
import Data.Text(Text, cons, singleton, snoc)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1, arbitraryBoundedEnum)

-- | Specify whether we write a value in 'UpperCase' or 'LowerCase'. The
-- 'Default' is 'UpperCase', since for example often Roman numerals are written
-- in /upper case/.
data LetterCase
  = UpperCase  -- ^ The /upper case/ formatting.
  | LowerCase  -- ^ The /lower case/ formatting.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Pick one of the two values based on the 'LetterCase' value.
splitLetterCase
  :: a -- ^ The value to return in case of 'UpperCase'.
  -> a -- ^ The value to return in case of 'LowerCase'.
  -> LetterCase -- ^ The given /letter case/.
  -> a -- ^ One of the two given values, depending on the 'LetterCase' value.
splitLetterCase x y = go
    where go UpperCase = x
          go LowerCase = y

-- | Specify whether we write a positive number /with/ or /without/ a plus sign.
-- the 'Default' is 'WithoutPlus'.
data PlusStyle
  = WithoutPlus -- ^ Write positive numbers /without/ using a plus sign.
  | WithPlus -- ^ Write positive numbers /with/ a plus sign.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Pick one of the two values based on the 't:PlusStyle' value.
splitPlusStyle
  :: a -- ^ The value to return in case of 'WithoutPlus'.
  -> a -- ^ The value to return in case of 'WithPlus'.
  -> PlusStyle -- ^ The plus style.
  -> a -- ^ One of the two given values, based on the 't:PlusStyle' value.
splitPlusStyle x y = go
  where go WithoutPlus = x
        go WithPlus = y

-- | The possible orientations of a unicode character, these can be
-- /horizontal/, or /vertical/.
data Orientation
  = Horizontal -- ^ /Horizontal/ orientation.
  | Vertical -- ^ /Vertical/ orientation.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A data type that specifies that an item has been given an orientation.
data Oriented a
  = Oriented {
    oobject :: a -- ^ The object that is oriented.
  , orientation :: Orientation -- ^ The oriented of the oriented object.
  } deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

-- | Possible rotations of a unicode character if that character can be rotated
-- over 0, 90, 180, and 270 degrees.
data Rotate90
  = R0 -- ^ No rotation.
  | R90 -- ^ Rotation over /90/ degrees.
  | R180 -- ^ Rotation over /180/ degrees.
  | R270 -- ^ Rotation over /270/ degrees.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A data type that lists the possible emphasis of a font. This can be 'Bold'
-- or 'NoBold' the 'Default' is 'NoBold'.
data Emphasis
  = NoBold -- ^ The characters are not stressed with boldface.
  | Bold -- ^ The characters are stressed in boldface.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Pick one of the two values based on the 't:Emphasis' value.
splitEmphasis
  :: a -- ^ The value to return in case of 'NoBold'.
  -> a -- ^ The value to return in case of 'Bold'.
  -> Emphasis -- ^ The emphasis type.
  -> a -- ^ One of the two given values, based on the 't:Emphasis' value.
splitEmphasis x y = go
  where go NoBold = x
        go Bold = y

-- | A data type that can be used to specify if an /italic/ character is used.
-- The 'Default' is 'NoItalic'.
data ItalicType
  = NoItalic -- ^ No italic characters are used.
  | Italic -- ^ Italic characters are used.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Pick one of the two values based on the 't:ItalicType' value.
splitItalicType
  :: a -- ^ The value to return in case of 'NoItalic'.
  -> a -- ^ The value to return in case of 'Italic'.
  -> ItalicType -- ^ The italic type.
  -> a -- ^ One of the two given values, based on the 't:ItalicType' value.
splitItalicType x y = go
  where go NoItalic = x
        go Italic = y

-- | A data type that specifies if the font is with /serifs/ or not. The
-- 'Defaul;t' is 'Serif'.
data FontStyle
  = SansSerif -- ^ The character is a character rendered /without/ serifs.
  | Serif -- ^ The character is a character rendered /with/ serifs.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Pick one of the two values based on the 't:FontStyle' value.
splitFontStyle
  :: a -- ^ The value to return in case of 'SansSerif'.
  -> a -- ^ The value to return in case of 'Serif'.
  -> FontStyle -- ^ The font style.
  -> a -- ^ One of the two given values, based on the 't:FontStyle' value.
splitFontStyle x y = go
  where go SansSerif = x
        go Serif = y

-- | Specify if one should ligate, or not. When litigation is done
-- characters that are normally written in two (or more) characters
-- are combined in one character. For example @Ⅲ@ instead of @ⅠⅠⅠ@.
data Ligate
  = Ligate -- ^ A ligate operation is performed on the characters, the 'def' for 't:Ligate'.
  | NoLigate -- ^ No ligate operation is performed on the charaters.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Pick one of the two values based on the value for 't:Ligate'.
splitLigate
  :: a -- ^ The value to return in case of 'v:Ligate'.
  -> a -- ^ The value to return in case of 'NoLigate'.
  -> Ligate -- ^ The ligation style.
  -> a -- ^ One of the two given values, based on the 't:Ligate' value.
splitLigate x y = go
    where go Ligate = x
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

-- | Calculate for a given plus and minus sign a 'Text' object for the given
-- number in the given 'PlusStyle'.
withSign :: Integral i
  => (i -> Text) -- ^ The function that maps the absolute value of the number to a 'Text' object that is appended to the sign.
  -> Char -- ^ The /plus/ sign to use.
  -> Char -- ^ The /minus/ sign to use.
  -> PlusStyle -- ^ The given 'PlusStyle' to use.
  -> i -- ^ The given 'Integral' number to render.
  -> Text -- ^ A 'Text' object that represents the given number, with the given sign numbers in the given 'PlusStyle'.
withSign f cp cn ps n | n < 0 = cons cn (f (-n))
                      | WithPlus <- ps = cons cp (f n)
                      | otherwise = f n

-- | A function to make it more convenient to implement a /sign-value system/.
-- This is done for a given /radix/ a function that maps the given value and the
-- given weight to a 'Text' object, a 'Text' object for /zero/ (since in some
-- systems that is different), and characters for /plus/ and /minus/.
-- The function then will for a given 'PlusStyle' convert the number to a
-- sequence of characters with respect to how the /sign-value system/ is
-- implemented.
signValueSystem :: Integral i
  => i  -- ^ The given /radix/ to use.
  -> (Int -> Int -> Text) -- ^ A function that maps the /value/ and the /weight/ to a 'Text' object.
  -> Text -- ^ The given 'Text' used to represent /zero/.
  -> Char -- ^ The given 'Char' used to denote /plus/.
  -> Char -- ^ The given 'Char' used to denote /minus/.
  -> PlusStyle -- ^ The given 'PlusStyle' to use.
  -> i -- ^ The given number to convert.
  -> Text -- ^ A 'Text' object that denotes the given number with the given /sign-value system/.
signValueSystem radix fi zero = withSign (f 0)
    where f 0 0 = zero
          f i n | n < radix = fi' n i
                | otherwise = f (i+1) q <> fi' r i
                where (q, r) = quotRem n radix
          fi' = flip fi . fromIntegral

-- | A function to make it more convenient to implement a /positional number
-- system/. This is done for a given /radix/ a given conversion funtion that
-- maps a value to a 'Char', and a 'Char' for /plus/ and /minus/.
-- The function then construct a 'Text' object for a given 'PlusStyle' and a given number.
positionalNumberSystem :: Integral i
  => i -- ^ The given radix to use.
  -> (Int -> Char) -- ^ A function that maps the value of a /digit/ to the corresponding 'Char'.
  -> Char -- ^ The given character used to denote /plus/.
  -> Char -- ^ The given character used to denote /minus/.
  -> PlusStyle -- ^ The given 'PlusStyle' to use.
  -> i -- ^ The given number to convert.
  -> Text -- ^ A 'Text' object that denotes the given number with the given /positional number system/.
positionalNumberSystem radix fi = withSign f
    where f n | n < radix = singleton (fi' n)
              | otherwise = snoc (f q) (fi' r)
              where (q, r) = quotRem n radix
          fi' = fi . fromIntegral

-- | A function to make it more convenient to implement a /positional number
-- system/ with /radix/ 10.
positionalNumberSystem10 :: Integral i
  => (Int -> Char) -- ^ A function that maps the value of a /digit/ to the corresponding 'Char'.
  -> Char -- ^ The given character used to denote /plus/.
  -> Char -- ^ The given character used to denote /minus/.
  -> PlusStyle -- ^ The given 'PlusStyle' to use.
  -> i -- ^ The given number to convert.
  -> Text -- ^ A 'Text' object that denotes the given number with the given /positional number system/.
positionalNumberSystem10 = positionalNumberSystem 10

instance Arbitrary LetterCase where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Orientation where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (Oriented a) where
    arbitrary = arbitrary1

instance Arbitrary1 Oriented where
    liftArbitrary arb = Oriented <$> arb <*> arbitrary

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

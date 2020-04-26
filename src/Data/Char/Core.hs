{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, Safe #-}

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
    -- * Ligating
  , Ligate(Ligate, NoLigate), ligate, ligateF
    -- * Types of fonts
  , Emphasis(NoBold, Bold), ItalicType(NoItalic, Italic), FontStyle(SansSerif, Serif)
    -- * Character range checks
  , isAsciiAlphaNum, isAsciiAlpha
    -- * Ways to display numbers
  , PlusStyle(WithoutPlus, WithPlus)
    -- * Functions to implement a number system
  , withSign
  ) where

import Data.Char(isAlpha, isAlphaNum, isAscii)
import Data.Default(Default(def))
import Data.Text(Text, cons)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1, arbitraryBoundedEnum)

-- | Specify whether we write a positive number /with/ or /without/ a plus sign.
-- the 'Default' is 'WithoutPlus'.
data PlusStyle
  = WithoutPlus -- ^ Write positive numbers /without/ using a plus sign.
  | WithPlus -- ^ Write positive numbers /with/ a plus sign.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

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

-- | A data type that can be used to specify if an /italic/ character is used.
-- The 'Default' is 'NoItalic'.
data ItalicType
  = NoItalic -- ^ No italic characters are used.
  | Italic -- ^ Italic characters are used.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A data type that specifies if the font is with /serifs/ or not. The
-- 'Defaul;t' is 'Serif'.
data FontStyle
  = SansSerif -- ^ The character is a character rendered /without/ serifs.
  | Serif -- ^ The character is a character rendered /with/ serifs.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Specify if one should ligate, or not. When litigation is done
-- characters that are normally written in two (or more) characters
-- are combined in one character. For example @Ⅲ@ instead of @ⅠⅠⅠ@.
data Ligate
  = Ligate -- ^ A ligate operation is performed on the characters.
  | NoLigate -- ^ No ligate operation is performed on the charaters.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

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

withSign :: Integral i => Char -> Char -> (i -> Text) -> PlusStyle -> i -> Text
withSign cp cn f ps n | n <= 0 = cons cn (f (-n))
                      | WithPlus <- ps = cons cp (f n)
                      | otherwise = f n

--positionalNumberSystem :: Integral i => Int -> (Int -> Char) -> PlusStyle -> i -> Text
--positionalNumberSystem radix toChar = go
--    where go Plus

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

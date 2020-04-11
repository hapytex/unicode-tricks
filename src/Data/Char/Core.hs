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
  ) where

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1, arbitraryBoundedEnum)

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

instance Arbitrary Orientation where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (Oriented a) where
    arbitrary = arbitrary1

instance Arbitrary1 Oriented where
    liftArbitrary arb = Oriented <$> arb <*> arbitrary

instance Arbitrary Rotate90 where
    arbitrary = arbitraryBoundedEnum

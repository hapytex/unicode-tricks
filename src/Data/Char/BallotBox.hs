{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, Safe #-}

{-|
Module      : Data.Char.BallotBox
Description : Support for the ballot box characters in unicode.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has a <https://www.unicode.org/charts/PDF/U2600.pdf block> named /Miscellaneous Symbols/ that includes unicode characters for boxes that are empty, contain a check or a cross, this module aims to make it more convenient to render these.
-}

module Data.Char.BallotBox (
      -- * Represent a ballot box.
      BallotBox(Empty, Check, Cross)
      -- * Convert a boolean to a ballot box.
    , toCheckBox, toCrossBox
  ) where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar', isInCharRange), UnicodeText(isInTextRange), generateIsInTextRange', mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_ballotOffset :: Int
_ballotOffset = 0x2610

-- | Check if the given 'Char'acter is a character that maps on a 'BallotBox' object.
isBallotBox
  :: Char  -- ^ The given 'Char'acter to check.
  -> Bool  -- ^ 'True' if the given 'Char'acter corresponds to a 'BallotBox' object; 'False' otherwise.
isBallotBox c = '\x2610' <= c && c <= '\x2612'

-- | A datatype that represents the different types of ballot boxes.
data BallotBox
  = Empty  -- ^ The box is /empty/, this is represented with ☐.
  | Check  -- ^ The box has a /check/, this is represented with ☑.
  | Cross  -- ^ The box has a /cross/, this is represented with ☒.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable BallotBox

instance NFData BallotBox

-- | Convert the given 'Bool'ean to a 'BallotBox' that is 'Empty', or contains a 'Check'.
toCheckBox
  :: Bool  -- ^ The given 'Bool' that determines if the box contains a 'Check'.
  -> BallotBox  -- ^ The corresponding 'BallotBox'.
toCheckBox = toEnum . fromEnum

-- | Convert the given 'Bool'ean to a 'BallotBox' that is 'Empty', or contains a 'Cross'.
toCrossBox
  :: Bool  -- ^ The given 'Bool' that determines if the box contains a 'Cross'.
  -> BallotBox  -- ^ The corresponding 'BallotBox'.
toCrossBox = toEnum . (2*) . fromEnum

instance Arbitrary BallotBox where
    arbitrary = arbitraryBoundedEnum

instance UnicodeCharacter BallotBox where
    toUnicodeChar = mapFromEnum _ballotOffset
    fromUnicodeChar = mapToEnumSafe _ballotOffset
    fromUnicodeChar' = mapToEnum _ballotOffset
    isInCharRange = isBallotBox

instance UnicodeText BallotBox where
    isInTextRange = generateIsInTextRange' @ BallotBox

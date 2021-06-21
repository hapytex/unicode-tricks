{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternSynonyms, Safe #-}

{-|
Module      : Data.Char.Emoji.Moon
Description : A module that defines moon emoji.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has two types of emoji for the moon: it contains eight emoji for the moonphase, and four
emoji where the moon has a face.
-}


module Data.Char.Emoji.Moon (
    -- * Moon phase emoji
    MoonPhase(NewMoon, WaxingCrescent, FirstQuarter, WaxingGibbous, FullMoon, WaningGibbous, ThirdQuarter, WaningCrescent)
  ) where

import Control.DeepSeq(NFData)

import Data.Char.Core(MirrorVertical(mirrorVertical), UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar'), UnicodeText, mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_moonPhaseOffset :: Int
_moonPhaseOffset = 0x1f311

-- | A data type that defines the eight different moon phases, and is an
-- instance of 'UnicodeCharacter' to convert these to the corresponding Unicode
-- character.
data MoonPhase
  = NewMoon  -- ^ The /new moon/, the first phase of the moon represented by 🌑.
  | WaxingCrescent  -- ^ The /waxing crescent/, the second phase of the moon represented by 🌒.
  | FirstQuarter  -- ^ The /first quarter/, the third phase of the moon represented by 🌓.
  | WaxingGibbous  -- ^ The /waxing gibbous/, the fourth phase of the moon represented by 🌔.
  | FullMoon  -- ^ The /full moon/, the fifth phase of the moon represented by 🌕.
  | WaningGibbous  -- ^ The /waning gibbous/, the sixth phase of the moon represented by 🌖.
  | ThirdQuarter  -- ^ The /third quarter/, the seventh phase of the moon represented by 🌗.
  | WaningCrescent  -- ^ The /waning crescent/, the eighth phase of the moon represented by 🌘.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary MoonPhase where
    arbitrary = arbitraryBoundedEnum

instance Hashable MoonPhase

instance MirrorVertical MoonPhase where
  mirrorVertical = toEnum . (`mod` 8) . (8 -) . fromEnum

instance NFData MoonPhase

instance UnicodeCharacter MoonPhase where
    toUnicodeChar = mapFromEnum _moonPhaseOffset
    fromUnicodeChar = mapToEnumSafe _moonPhaseOffset
    fromUnicodeChar' = mapToEnum _moonPhaseOffset

instance UnicodeText MoonPhase

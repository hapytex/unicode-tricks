{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.Char.Emoji.Moon
-- Description : A module that defines moon emoji.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Unicode has two types of emoji for the moon: it contains eight emoji for the moonphase, and four
-- emoji where the moon has a face.
module Data.Char.Emoji.Moon
  ( -- * Moon phase emoji
    MoonPhase (NewMoon, WaxingCrescent, FirstQuarter, WaxingGibbous, FullMoon, WaningGibbous, ThirdQuarter, WaningCrescent),

    -- * Moon faces emoji
    MoonFace (NewMoonFace, FirstQuarterFace, FullMoonFace, ThirdQuarterFace),
    moonPhaseForDay,
  )
where

import Control.DeepSeq (NFData)
import Data.Char.Core (MirrorVertical (mirrorVertical), UnicodeCharacter (fromUnicodeChar, fromUnicodeChar', isInCharRange, toUnicodeChar), UnicodeText (isInTextRange), generateIsInTextRange', mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Time.Calendar (Day (toModifiedJulianDay))
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)

_moonPhaseOffset :: Int
_moonPhaseOffset = 0x1f311

-- | A data type that defines the eight different moon phases, and is an
-- instance of 'UnicodeCharacter' to convert these to the corresponding Unicode
-- character.
data MoonPhase
  = -- | The /new moon/, the first phase of the moon represented by 🌑.
    NewMoon
  | -- | The /waxing crescent/, the second phase of the moon represented by 🌒.
    WaxingCrescent
  | -- | The /first quarter/, the third phase of the moon represented by 🌓.
    FirstQuarter
  | -- | The /waxing gibbous/, the fourth phase of the moon represented by 🌔.
    WaxingGibbous
  | -- | The /full moon/, the fifth phase of the moon represented by 🌕.
    FullMoon
  | -- | The /waning gibbous/, the sixth phase of the moon represented by 🌖.
    WaningGibbous
  | -- | The /third quarter/, the seventh phase of the moon represented by 🌗.
    ThirdQuarter
  | -- | The /waning crescent/, the eighth phase of the moon represented by 🌘.
    WaningCrescent
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

-- | Determine the corresponding MoonPhase emoji for a given day. The algorithm is based on
-- upon a subsystems publication <https://www.subsystems.us/uploads/9/8/9/4/98948044/moonphase.pdf>
moonPhaseForDay ::
  -- | The 'Day' for which we want to deterime the moon phase.
  Day ->
  -- | The corresponding 'MoonPhase' icon
  MoonPhase
moonPhaseForDay d = toEnum (round (((fromIntegral (toModifiedJulianDay d - 57812) :: Double) + 0.845625) / 3.69125) `mod` 8)

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
  isInCharRange c = '\x1f311' <= c && c <= '\x1f318'

instance UnicodeText MoonPhase where
  isInTextRange = generateIsInTextRange' @MoonPhase

-- | A data type that defines the four different moon faces (not to be confused with
-- phases). This data type is an instance of the 'UnicodeCharacter' type class
-- to convert these to the corresponding Unicode character.
data MoonFace
  = -- | The /new moon/, the first phase of the moon faces represented by 🌚.
    NewMoonFace
  | -- | The /first quarter/, the second phase of the moon faces represented by 🌛.
    FirstQuarterFace
  | -- | The /full moon/, the third phase of the moon faces represented by 🌝.
    FullMoonFace
  | -- | The /third quarter/, the fourth phase of the moon faces represented by 🌜.
    ThirdQuarterFace
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary MoonFace where
  arbitrary = arbitraryBoundedEnum

instance Hashable MoonFace

instance MirrorVertical MoonFace where
  mirrorVertical = toEnum . (3 -) . fromEnum

instance NFData MoonFace

instance UnicodeCharacter MoonFace where
  toUnicodeChar NewMoonFace = '\x1f31a'
  toUnicodeChar FirstQuarterFace = '\x1f31b'
  toUnicodeChar FullMoonFace = '\x1F31d'
  toUnicodeChar ThirdQuarterFace = '\x1f31c'
  fromUnicodeChar '\x1f31a' = Just NewMoonFace
  fromUnicodeChar '\x1f31b' = Just FirstQuarterFace
  fromUnicodeChar '\x1f31d' = Just FullMoonFace
  fromUnicodeChar '\x1f31c' = Just ThirdQuarterFace
  fromUnicodeChar _ = Nothing
  isInCharRange c = '\x1f31a' <= c && c <= '\x1f31d'

instance UnicodeText MoonFace where
  isInTextRange = generateIsInTextRange' @MoonFace

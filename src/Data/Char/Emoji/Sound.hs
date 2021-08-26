{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, Safe, TypeApplications #-}

{-|
Module      : Data.Char.Emoji.Sound
Description : A module that defines emoji that deal with sound, volume and instruments.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

The emoji have nine sound emoji. This modules defines data constructors for these emoji, and aims to
define a sensical 'Ord' relation for example to specify the volume of sound.
-}

module Data.Char.Emoji.Sound where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar', isInCharRange), UnicodeText(isInTextRange), mapFromEnum, mapToEnumSafe, mapToEnum, generateIsInTextRange')
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_volumeOffset :: Int
_volumeOffset = 0x1f507

-- ^ There are four emoji that specify the volume of sound.
data Volume
  = SilentVolume  -- ^ The volume of the sound is set to /silence/, this is denoted with 🔇.
  | LowVolume  -- ^ The volume of the sound is /low/, this is denoted with 🔈.
  | MediumVolume  -- ^ The volume of the sound is /medium/, this is denoted with 🔉.
  | LoudVolume  -- ^ The volume of the sound is set to /loud/, this is denoted with 🔊.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary Volume where
  arbitrary = arbitraryBoundedEnum

instance Hashable Volume

instance NFData Volume

instance UnicodeCharacter Volume where
  toUnicodeChar = mapFromEnum _volumeOffset
  fromUnicodeChar = mapToEnumSafe _volumeOffset
  fromUnicodeChar' = mapToEnum _volumeOffset
  isInCharRange c = '\x1f507' <= c && c <= '\x1f50a'

instance UnicodeText Volume where
  isInTextRange = generateIsInTextRange' @Volume

-- | There are two bell emoji, one with a bell, and one without a bell. This is often used
-- to enable/disable notifications.
data Bell
  = NoBell  -- ^ An emoji with a bell and a slash, often used to disable notifcations and denoted with 🔕.
  | Bell  -- ^ An emoji with a bell, often used to enable notifications and denoted with 🔔.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary Bell where
  arbitrary = arbitraryBoundedEnum

instance Hashable Bell

instance NFData Bell

instance UnicodeCharacter Bell where
  toUnicodeChar NoBell = '\x1f515'
  toUnicodeChar Bell = '\x1f514'
  fromUnicodeChar' '\x1f515' = NoBell
  fromUnicodeChar' '\x1f514' = Bell
  fromUnicodeChar '\x1f515' = Just NoBell
  fromUnicodeChar '\x1f514' = Just Bell
  fromUnicodeChar _ = Nothing
  isInCharRange c = '\x1f514' <= c && c <= '\x1f515'

instance UnicodeText Bell where
  isInTextRange = generateIsInTextRange' @Bell

-- ^ Three emoji that deal with amplifying sound.
data Instrument
  = Loudspeaker  -- ^ The /loudspeaker/ emoji, denoted with 📢.
  | Megaphone  -- ^ The /megaphone/ emoji, denoted with 📣.
  | PostalHorn  -- ^ The /postal horn/ emoji, denoted with 📯.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary Instrument where
  arbitrary = arbitraryBoundedEnum

instance Hashable Instrument

instance NFData Instrument

instance UnicodeCharacter Instrument where
  toUnicodeChar Loudspeaker = '\x1f4e2'
  toUnicodeChar Megaphone = '\x1f4e3'
  toUnicodeChar PostalHorn = '\x1f4ef'
  fromUnicodeChar' '\x1f4e2' = Loudspeaker
  fromUnicodeChar' '\x1f4e3' = Megaphone
  fromUnicodeChar' '\x1f4ef' = PostalHorn
  fromUnicodeChar '\x1f4e2' = Just Loudspeaker
  fromUnicodeChar '\x1f4e3' = Just Megaphone
  fromUnicodeChar '\x1f4ef' = Just PostalHorn
  fromUnicodeChar _ = Nothing
  isInCharRange '\x1f4ef' = True
  isInCharRange c = '\x1f4e2' <= c && c <= '\x1f4e3'


instance UnicodeText Instrument where
  isInTextRange = generateIsInTextRange' @Instrument

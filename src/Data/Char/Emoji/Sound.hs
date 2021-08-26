{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, Safe, TypeApplications #-}

module Data.Char.Emoji.Sound where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar', isInCharRange), UnicodeText(isInTextRange), mapFromEnum, mapToEnumSafe, mapToEnum, generateIsInTextRange')
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_volumeOffset :: Int
_volumeOffset = 0x1f507

data Volume
  = SilentVolume
  | LowVolume
  | MediumVolume
  | LoudVolume
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

data Bell
  = NoBell
  | Bell
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

data Sound
  = Loudspeaker
  | Megaphone
  | PostalHorn
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary Sound where
  arbitrary = arbitraryBoundedEnum

instance Hashable Sound

instance NFData Sound

instance UnicodeCharacter Sound

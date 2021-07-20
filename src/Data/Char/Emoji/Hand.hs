{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternSynonyms, Safe #-}

{-|
Module      : Data.Char.Emoji.Hand
Description : A module that provides Emojis about hands and fingers.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX


-}

module Data.Char.Emoji.Hand (
    SingleCharHandGesture(
        WavingHand, RaisedBackOfHand, RaisedHand, VulcanSalute, OkHandSign
      , PinchedFingers, PinchingHand, CrossedFingers, LoveYouGesture, SignOfTheHorns, CallMeHand
    ),
    pattern FingersCrossed, pattern SpockHand, pattern HornsSign
  ) where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar), UnicodeText)
import Data.Char.Emoji.SkinColor(WithSkinColorModifierUnicodeText)
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- | A datatype that constructs /hand gestures/ that correspond with a /single/ 'Char'acter.
data SingleCharHandGesture
  = WavingHand  -- ^ A waving hand, this is denoted with 👋.
  | RaisedBackOfHand  -- ^ The raised back of a hand, this is denoted with 🤚.
  | RaisedHand  -- ^ A raised hand, this is denoted with ✋.
  | VulcanSalute  -- ^ The /Vulcan/ salute, this is denoted with 🖖.
  | OkHandSign  -- ^ The okay hand sign, this is denoted with 👌.
  | PinchedFingers  -- ^
  | PinchingHand
  | CrossedFingers
  | LoveYouGesture
  | SignOfTheHorns
  | CallMeHand
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary SingleCharHandGesture where
  arbitrary = arbitraryBoundedEnum

instance Hashable SingleCharHandGesture

instance NFData SingleCharHandGesture

instance UnicodeCharacter SingleCharHandGesture where
  toUnicodeChar WavingHand = '\x1f44b'
  toUnicodeChar RaisedBackOfHand = '\x1f91a'
  toUnicodeChar RaisedHand = '\x270b'
  toUnicodeChar VulcanSalute = '\x1f596'
  toUnicodeChar OkHandSign = '\x1f44c'
  toUnicodeChar PinchedFingers = '\x1f90c'
  toUnicodeChar PinchingHand = '\x1f90f'
  toUnicodeChar CrossedFingers = '\x1f91e'
  toUnicodeChar LoveYouGesture = '\x1f91f'
  toUnicodeChar SignOfTheHorns = '\x1f918'
  toUnicodeChar CallMeHand = '\x1f919'
  fromUnicodeChar '\x1f44b' = Just WavingHand
  fromUnicodeChar '\x1f91a' = Just RaisedBackOfHand
  fromUnicodeChar '\x270b' = Just RaisedHand
  fromUnicodeChar '\x1f596' = Just VulcanSalute
  fromUnicodeChar '\x1f44c' = Just OkHandSign
  fromUnicodeChar '\x1f90c' = Just PinchedFingers
  fromUnicodeChar '\x1f90f' = Just PinchingHand
  fromUnicodeChar '\x1f91e' = Just CrossedFingers
  fromUnicodeChar '\x1f91f' = Just LoveYouGesture
  fromUnicodeChar '\x1f918' = Just SignOfTheHorns
  fromUnicodeChar '\x1f919' = Just CallMeHand
  fromUnicodeChar _ = Nothing

instance UnicodeText SingleCharHandGesture

instance WithSkinColorModifierUnicodeText SingleCharHandGesture

data MultiCharHandGesture
  = RaisedHandWithFingersSplayed
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

-- | A pattern synonym for 'CrossedFingers'.
pattern FingersCrossed :: SingleCharHandGesture
pattern FingersCrossed = CrossedFingers

-- | A pattern synonym for the 'VulcanSalute'.
pattern SpockHand :: SingleCharHandGesture
pattern SpockHand = VulcanSalute

-- | A pattern synonym for 'SignOfTheHorns'.
pattern HornsSign :: SingleCharHandGesture
pattern HornsSign = SignOfTheHorns

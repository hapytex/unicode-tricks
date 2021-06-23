{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternSynonyms, Safe #-}

module Data.Char.Emoji.Hand (
    HandGesture(
        WavingHand, RaisedBackOfHand, RaisedHandWithFingersSplayed, RaisedHand, VulcanSalute, OkHandSign
      , PinchedFingers, PinchingHand, CrossedFingers, LoveYouGesture, SignOfTheHorns, CallMeHand
    ),
    pattern FingersCrossed, pattern SpockHand, pattern HornsSign
  ) where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar), UnicodeText)
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

data HandGesture
  = WavingHand
  | RaisedBackOfHand
  | RaisedHandWithFingersSplayed
  | RaisedHand
  | VulcanSalute
  | OkHandSign
  | PinchedFingers
  | PinchingHand
  | CrossedFingers
  | LoveYouGesture
  | SignOfTheHorns
  | CallMeHand
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary HandGesture where
  arbitrary = arbitraryBoundedEnum

instance Hashable HandGesture

instance NFData HandGesture

instance UnicodeCharacter HandGesture where
  toUnicodeChar WavingHand = '\x1f44b'
  toUnicodeChar RaisedBackOfHand = '\x1f91a'
  toUnicodeChar RaisedHandWithFingersSplayed = '\x1f590'
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
  fromUnicodeChar '\x1f590' = Just RaisedHandWithFingersSplayed
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

instance UnicodeText HandGesture

-- TODO: victory hand


pattern FingersCrossed :: HandGesture
pattern FingersCrossed = CrossedFingers

pattern SpockHand :: HandGesture
pattern SpockHand = VulcanSalute

pattern HornsSign :: HandGesture
pattern HornsSign = SignOfTheHorns

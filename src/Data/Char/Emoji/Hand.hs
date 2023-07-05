{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.Char.Emoji.Hand
-- Description : A module that provides Emojis about hands and fingers.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Unicode has emoji's for hands. In this module we make it more convenient
-- to render hand gestures with a specific skin color.
module Data.Char.Emoji.Hand
  ( SingleCharHandGesture
      ( WavingHand,
        RaisedBackOfHand,
        RaisedHand,
        VulcanSalute,
        OkHandSign,
        PinchedFingers,
        PinchingHand,
        CrossedFingers,
        LoveYouGesture,
        SignOfTheHorns,
        CallMeHand
      ),
    MultiCharHandGesture,
    pattern FingersCrossed,
    pattern SpockHand,
    pattern HornsSign,
  )
where

import Control.DeepSeq (NFData)
import Data.Char.Core (UnicodeCharacter (fromUnicodeChar, isInCharRange, toUnicodeChar), UnicodeText (fromUnicodeText, isInTextRange, toUnicodeText), generateIsInTextRange')
import Data.Char.Emoji.SkinColor (WithSkinColorModifierUnicodeText)
import Data.Data (Data)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)

-- | A datatype that constructs /hand gestures/ that correspond with a /single/ 'Char'acter.
data SingleCharHandGesture
  = -- | A waving hand, this is denoted with 👋.
    WavingHand
  | -- | The raised back of a hand, this is denoted with 🤚.
    RaisedBackOfHand
  | -- | A raised hand, this is denoted with ✋.
    RaisedHand
  | -- | The /Vulcan/ salute, this is denoted with 🖖.
    VulcanSalute
  | -- | The okay hand sign, this is denoted with 👌.
    OkHandSign
  | -- |  The /pinched fingers/ gesture, this is denoted with 🤌.
    PinchedFingers
  | -- | The /pinching hand/ gesture, this is denoted with 🤏.
    PinchingHand
  | -- | The /crossed fingers/ gesture, this is denoted with 🤞.
    CrossedFingers
  | -- | The /love you/ gesture, this is denoted with 🤟.
    LoveYouGesture
  | -- | The sign of the horns, this is denoted with 🤘.
    SignOfTheHorns
  | -- | The /call me/ hand sign, this is denoted with 🤙.
    CallMeHand
  | -- | A middle finger pointing up, this is denoted with 🖕.
    MiddleFinger
  | -- | An emoji where the thumb is pointing upwards, this is denoted with 👍.
    ThumbsUp
  | -- | An emoji where the thumb is pointing downwards, this is denoted with 👎.
    ThumbsDown
  | -- | An emoji where the fist is rased, this is denoted with ✊.
    RaisedFist
  | -- | An emoji of a fisted hand, this is denoted with 👊.
    FistedHand
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
  toUnicodeChar MiddleFinger = '\x1f595'
  toUnicodeChar ThumbsUp = '\x1f44d'
  toUnicodeChar ThumbsDown = '\x1f44e'
  toUnicodeChar RaisedFist = '\x270a'
  toUnicodeChar FistedHand = '\x1f44a'
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
  fromUnicodeChar '\x1f595' = Just MiddleFinger
  fromUnicodeChar '\x1f44d' = Just ThumbsUp
  fromUnicodeChar '\x1f44e' = Just ThumbsDown
  fromUnicodeChar '\x270a' = Just RaisedFist
  fromUnicodeChar '\x1f44a' = Just FistedHand
  fromUnicodeChar _ = Nothing
  isInCharRange '\x1f44b' = True
  isInCharRange '\x1f91a' = True
  isInCharRange '\x270b' = True
  isInCharRange '\x1f596' = True
  isInCharRange '\x1f44c' = True
  isInCharRange '\x1f90c' = True
  isInCharRange '\x1f90f' = True
  isInCharRange '\x1f91e' = True
  isInCharRange '\x1f91f' = True
  isInCharRange '\x1f918' = True
  isInCharRange '\x1f919' = True
  isInCharRange '\x1f595' = True
  isInCharRange '\x1f44d' = True
  isInCharRange '\x1f44e' = True
  isInCharRange '\x270a' = True
  isInCharRange '\x1f44a' = True
  isInCharRange _ = False

instance UnicodeText SingleCharHandGesture where
  isInTextRange = generateIsInTextRange' @SingleCharHandGesture

instance WithSkinColorModifierUnicodeText SingleCharHandGesture

-- | Emoji with hands that map on a /sequence/ of characters instead of one character.
data MultiCharHandGesture
  = -- | The raised hand with fingers splayed emoji, this is denoted as 🖐️.
    RaisedHandWithFingersSplayed
  | -- | The /victory hand/ emoji, this is denoted as ✌️.
    VictoryHand
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary MultiCharHandGesture where
  arbitrary = arbitraryBoundedEnum

instance Hashable MultiCharHandGesture

instance NFData MultiCharHandGesture

instance UnicodeText MultiCharHandGesture where
  toUnicodeText RaisedHandWithFingersSplayed = "\x1f590\xfe0f"
  toUnicodeText VictoryHand = "\x270c\xfe0f"
  fromUnicodeText "\x1f590\xfe0f" = Just RaisedHandWithFingersSplayed
  fromUnicodeText "\x270c\xfe0f" = Just VictoryHand
  fromUnicodeText _ = Nothing
  isInTextRange "\x1f590\xfe0f" = True
  isInTextRange "\x270c\xfe0f" = True
  isInTextRange _ = False

instance WithSkinColorModifierUnicodeText MultiCharHandGesture

-- | A pattern synonym for 'CrossedFingers'.
pattern FingersCrossed :: SingleCharHandGesture
pattern FingersCrossed = CrossedFingers

-- | A pattern synonym for the 'VulcanSalute'.
pattern SpockHand :: SingleCharHandGesture
pattern SpockHand = VulcanSalute

-- | A pattern synonym for 'SignOfTheHorns'.
pattern HornsSign :: SingleCharHandGesture
pattern HornsSign = SignOfTheHorns

{-# LANGUAGE PatternSynonyms, Safe #-}

module Data.Char.Emoji.Hand (
  ) where

data HandGesture
  = WavingHand
  | RaisedBackOfHand
  | RaisedHandWithFingersSplayed
  | RaisedHand
  | VulcanSalute
  | OkHandSign
  | PinchedFingers
  | PinchedHand
  | VictoryHand
  | CrossedFingers
  | LoveYouGesture
  | Horns

pattern FingersCrossed :: HandGesture
pattern FingersCrossed = CrossedFingers

pattern SpockHand :: HandGesture
pattern SpockHand = RaisedHandWithPartBetweenMiddleAndRingFinger

{-# LANGUAGE TypeApplications #-}

module Data.Char.CombiningSpec
  ( spec
  ) where

import Data.Char.Combining
import Data.Maybe
import Data.Char.CoreTest

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "isCombining" $ do
    it "is equivalent to combiningChar being Just" $ property $
      \c -> isCombiningCharacter c == isJust (combiningCharacter c)
    it "all combining characters are combining characters" $ property $
      isCombiningCharacter . combiningToUnicode
  describe "combiningCharacter" $
    it "combiningCharacter and combiningToUnicode are each others inverse" $ property $
      \c -> combiningCharacter (combiningToUnicode c) == Just c
  testUnicodeCharacter @ CombiningCharacter
  testUnicodeText @ CombiningCharacter
  testHashable @ CombiningCharacter
  testHashable @ CombiningSequence

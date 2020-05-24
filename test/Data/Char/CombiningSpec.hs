module Data.Char.CombiningSpec
  ( spec
  ) where

import Data.Char.Combining
import Data.Maybe

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "isCombining" $ do
    it "is equivalent to combiningChar being Just" $ property $
      \c -> isCombiningCharacter c == isJust (combiningCharacter c)
    it "all combining characters are combining characters" $ property $
      isCombiningCharacter . combiningToUnicode

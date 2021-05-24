{-# LANGUAGE TypeApplications #-}

module Data.Char.CardSpec
  ( spec
  ) where

import Data.Char.Card
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @ CardSuit
    testUnicodeText @ CardSuit
    testHashable @ CardSuit
    testHashable @ CardRank
    testHashable @ JokerColor
    testHashable @ Trump
    testHashable @ Card

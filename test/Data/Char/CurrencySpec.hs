{-# LANGUAGE TypeApplications #-}

module Data.Char.CurrencySpec
  ( spec
  ) where

import Data.Char.Currency
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @ Currency
    testUnicodeText @ Currency
    testHashable @ Currency

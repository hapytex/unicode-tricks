{-# LANGUAGE TypeApplications #-}

module Data.Char.Block.SextantSpec
  ( spec
  ) where

import Data.Char.Block.Sextant(Sextant)
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter @ (Sextant Bool)
    testUnicodeText @ (Sextant Bool)
    testHashable @ (Sextant Bool)

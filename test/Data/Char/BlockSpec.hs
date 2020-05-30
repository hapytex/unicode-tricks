module Data.Char.BlockSpec
  ( spec
  ) where

import Data.Char.Block
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter (undefined :: Block Bool)
    testUnicodeText (undefined :: Block Bool)

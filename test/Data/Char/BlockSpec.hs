module Data.Char.BlockSpec
  ( spec
  ) where

import Data.Char.Core
import Data.Char.Block
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = testUnicodeCharacter (undefined :: Block Bool)

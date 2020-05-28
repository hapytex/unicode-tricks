module Data.Char.FrameSpec
  ( spec
  ) where

import Data.Char.Frame
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter (undefined :: Parts Weight)
    testUnicodeCharacter (undefined :: Parts Bool)

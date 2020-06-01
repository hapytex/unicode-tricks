module Data.Char.FrameSpec
  ( spec
  ) where

import Data.Char.Frame
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter "Parts Weight" (undefined :: Parts Weight)
    testUnicodeCharacter "Parts Bool" (undefined :: Parts Bool)
    testUnicodeText "Parts Weight" (undefined :: Parts Weight)
    testUnicodeText "Parts Bool" (undefined :: Parts Bool)

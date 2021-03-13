module Data.Char.BallotBoxSpec
  ( spec
  ) where

import Data.Char.BallotBox
import Data.Char.CoreTest
import Test.Hspec

spec :: Spec
spec = do
    testUnicodeCharacter "BallotBox" (undefined :: BallotBox)
    testUnicodeText "BallotBox" (undefined :: BallotBox)

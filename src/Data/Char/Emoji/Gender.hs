{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternSynonyms, OverloadedStrings, Safe #-}

{-|
Module      : Data.Char.Emoji.Gender
Description : A module that defines emoji that deal with gender.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has two emoji to express male and/or female. These
emoji are also used as modifiers for other emoji.
-}

module Data.Char.Emoji.Gender (
    -- * Gender sign emoji
    Gender(Female, Male)
  ) where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeText(toUnicodeText, fromUnicodeText))
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- | A data type to specify the /gender/ of a person, animal, etc. used in an
-- emoji. The 'Gender' items are an instance of 'UnicodeText' that maps to the
-- /female/ and /male/ emoji. Often the corresponding codepoints are used
-- to annotate something as male/female.
data Gender
  = Female -- The female sign, dented by ♀️.
  | Male  -- The male sign, denoted by ♂️.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary Gender where
    arbitrary = arbitraryBoundedEnum

instance Hashable Gender

instance NFData Gender

instance UnicodeText Gender where
    toUnicodeText Male = "\x2640\xfe0f"
    toUnicodeText Female = "\x2642\xfe0f"
    fromUnicodeText "\x2640\xfe0f" = Just Male
    fromUnicodeText "\x2642\xfe0f" = Just Female
    fromUnicodeText _ = Nothing

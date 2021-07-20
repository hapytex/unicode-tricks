{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, Safe #-}

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
    BinaryGender(Female, Male)
  , Trigender(Binary, Transgender)
  ) where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeText(toUnicodeText, fromUnicodeText))
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- | A data type to specify the /gender/ of a person, animal, etc. used in an
-- emoji. The 'Data.Char.Emoji.BinaryGender' items are an instance of 'UnicodeText' that maps to the
-- /female/ and /male/ emoji. Often the corresponding codepoints are used
-- to annotate something as male/female.
data BinaryGender
  = Female -- The female sign, dented by ♀️.
  | Male  -- The male sign, denoted by ♂️.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary BinaryGender where
    arbitrary = arbitraryBoundedEnum

instance Hashable BinaryGender

instance NFData BinaryGender

instance UnicodeText BinaryGender where
    toUnicodeText Male = "\x2640\xfe0f"
    toUnicodeText Female = "\x2642\xfe0f"
    fromUnicodeText "\x2640\xfe0f" = Just Male
    fromUnicodeText "\x2642\xfe0f" = Just Female
    fromUnicodeText _ = Nothing

-- | A data type that, besides 'Male' and 'Female' can also represent a 'Transgender'.
data Trigender
  = Binary BinaryGender  -- ^ Specify a /binary/ gender which is /female/ or /male/.
  | Transgender  -- ^ A value that specifies a /transgender/, this is denoted with ⚧️.
  deriving (Data, Eq, Generic, Ord, Read, Show)

instance Bounded Trigender where
  minBound = Binary minBound
  maxBound = Transgender

instance Enum Trigender where
  fromEnum (Binary g) = fromEnum g
  fromEnum Transgender = 2
  toEnum 2 = Transgender
  toEnum x = Binary (toEnum x)

instance Arbitrary Trigender where
    arbitrary = arbitraryBoundedEnum

instance Hashable Trigender

instance NFData Trigender

instance UnicodeText Trigender where
    toUnicodeText (Binary g) = toUnicodeText g
    toUnicodeText Transgender = "\x26a7\xfe0f"
    fromUnicodeText "\x26a7\xfe0f" = Just Transgender
    fromUnicodeText x = Binary <$> fromUnicodeText x

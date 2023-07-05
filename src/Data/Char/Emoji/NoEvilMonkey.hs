{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.Char.Emoji.NoEvilMonkey
-- Description : Support for the /no-evil monkeys in the Unicode standard.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Unicode has three codepoints that refer to characters of monkeys where the first one does not see, the second does not hear, and the third one does not speak.
module Data.Char.Emoji.NoEvilMonkey
  ( NoEvilMonkey (SeeNoEvilMonkey, HearNoEvilMonkey, SpeakNoEvilMonkey),
  )
where

import Control.DeepSeq (NFData)
import Data.Char.Core (UnicodeCharacter (fromUnicodeChar, fromUnicodeChar', isInCharRange, toUnicodeChar), UnicodeText (isInTextRange), generateIsInTextRange', mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data (Data)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)

_noEvilMonkeyOffset :: Int
_noEvilMonkeyOffset = 0x1f648

-- | A data constructors that lists the three different /no-evil/ monkeys.
data NoEvilMonkey
  = -- | The /see-no-evil monkey/, denoted with 🙈.
    SeeNoEvilMonkey
  | -- | The /hear-no-evil monkey/, denoted with 🙉.
    HearNoEvilMonkey
  | -- | The /speak-no-evil monkey/, denoted with 🙊.
    SpeakNoEvilMonkey
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable NoEvilMonkey

instance NFData NoEvilMonkey

instance Arbitrary NoEvilMonkey where
  arbitrary = arbitraryBoundedEnum

instance UnicodeCharacter NoEvilMonkey where
  toUnicodeChar = mapFromEnum _noEvilMonkeyOffset
  fromUnicodeChar = mapToEnumSafe _noEvilMonkeyOffset
  fromUnicodeChar' = mapToEnum _noEvilMonkeyOffset
  isInCharRange c = '\x1f648' <= c && c <= '\x1f64a'

instance UnicodeText NoEvilMonkey where
  isInTextRange = generateIsInTextRange' @NoEvilMonkey

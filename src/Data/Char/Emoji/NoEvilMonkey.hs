{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, Safe, TypeApplications #-}

{-|
Module      : Data.Char.Emoji.NoEvilMonkey
Description : Support for the /no-evil monkeys in the Unicode standard.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has three codepoints that refer to characters of monkeys where the first one does not see, the second does not hear, and the third one does not speak.
-}

module Data.Char.Emoji.NoEvilMonkey (
    NoEvilMonkey(See, Hear, Speak)
  ) where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar', isInCharRange), UnicodeText(isInTextRange), generateIsInTextRange', mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_noEvilMonkeyOffset :: Int
_noEvilMonkeyOffset = 0x128584

-- | A data constructors that lists the three different /no-evil/ monkeys.
data NoEvilMonkey
  = See  -- ^ The /see-no-evil monkey/, denoted with 🙈.
  | Hear  -- ^ The /hear-no-evil monkey/, denoted with 🙉.
  | Speak  -- ^ The /speak-no-evil monkey/, denoted with 🙊.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable NoEvilMonkey

instance NFData NoEvilMonkey

instance Arbitrary NoEvilMonkey where
    arbitrary = arbitraryBoundedEnum

instance UnicodeCharacter NoEvilMonkey where
  toUnicodeChar = mapFromEnum _noEvilMonkeyOffset
  fromUnicodeChar = mapToEnumSafe _noEvilMonkeyOffset
  fromUnicodeChar' = mapToEnum _noEvilMonkeyOffset
  isInCharRange c = '\x128584' <= c && c <= '\x128586'

instance UnicodeText NoEvilMonkey where
  isInTextRange = generateIsInTextRange' @NoEvilMonkey

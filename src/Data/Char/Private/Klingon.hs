{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.Char.Private.Klingon
-- Description : Support for Klingon, which is sometimes implemented in the /private usage area/.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Some fonts implement the /Klingon script/ characters with the /private usage area/. <https://en.wikipedia.org/wiki/Klingon_scripts Wikipedia>
-- describes the different fonts and <https://www.evertype.com/standards/csur/klingon.html Evertype> provides a font to typeset Klingon script.
module Data.Char.Private.Klingon
  ( -- * Representing Klingon characters
    Klingon
      ( A,
        B,
        Ch,
        D,
        E,
        Gh,
        H,
        I,
        J,
        L,
        M,
        N,
        Ng,
        O,
        P,
        Q,
        QUpper,
        R,
        S,
        T,
        Tlh,
        U,
        V,
        W,
        Y,
        GlottalStop,
        Zero,
        One,
        Two,
        Three,
        Four,
        Five,
        Six,
        Seven,
        Eight,
        Nine,
        Comma,
        FullStop,
        Mummification
      ),
  )
where

import Control.DeepSeq (NFData)
import Data.Char (chr, ord)
import Data.Char.Core (UnicodeCharacter (fromUnicodeChar, fromUnicodeChar', isInCharRange, toUnicodeChar), UnicodeText (isInTextRange), generateIsInTextRange')
import Data.Data (Data)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary), arbitraryBoundedEnum)

-- | A datatype to represent the Klingon characters, and their mapping to a Klingon font.
data Klingon
  = -- | The @a@ character in Klingon script.
    A
  | -- | The @b@ character in Klingon script.
    B
  | -- | The @ch@ character in Klingon script.
    Ch
  | -- | The @D@ character in Klingon script.
    D
  | -- | The @e@ character in Klingon script.
    E
  | -- | The @gh@ character in Klingon script.
    Gh
  | -- | The @H@ character in Klingon script.
    H
  | -- | The @I@ character in Klingon script.
    I
  | -- | The @j@ character in Klingon script.
    J
  | -- | The @l@ character in Klingon script.
    L
  | -- | The @m@ character in Klingon script.
    M
  | -- | The @n@ character in Klingon script.
    N
  | -- | The @ng@ character in Klingon script.
    Ng
  | -- | The @o@ character in Klingon script.
    O
  | -- | The @p@ character in Klingon script.
    P
  | -- | The @q@ character in Klingon script.
    Q
  | -- | The @Q@ character in Klingon script.
    QUpper
  | -- | The @r@ character in Klingon script.
    R
  | -- | The @S@ character in Klingon script.
    S
  | -- | The @t@ character in Klingon script.
    T
  | -- | The @tlh@ character in Klingon script.
    Tlh
  | -- | The @u@ character in Klingon script.
    U
  | -- | The @v@ character in Klingon script.
    V
  | -- | The @w@ character in Klingon script.
    W
  | -- | The @y@ character in Klingon script.
    Y
  | -- | The @ʼ@ character in Klingon script, denoting the /glottal stop/.
    GlottalStop
  | -- | The @0@ character in Klingon script.
    Zero
  | -- | The @1@ character in Klingon script.
    One
  | -- | The @2@ character in Klingon script.
    Two
  | -- | The @3@ character in Klingon script.
    Three
  | -- | The @4@ character in Klingon script.
    Four
  | -- | The @5@ character in Klingon script.
    Five
  | -- | The @6@ character in Klingon script.
    Six
  | -- | The @7@ character in Klingon script.
    Seven
  | -- | The @8@ character in Klingon script.
    Eight
  | -- | The @9@ character in Klingon script.
    Nine
  | -- | The /comma/ character in Klingon script, denoted by an /up-turned triangle/.
    Comma
  | -- | The /full stop/ character in Klingon script, denoted by a /down-turned triangle/.
    FullStop
  | -- | The /mummification/ character in Klingon script, also known as the /klingon character for the empire/ or /heart of virtue/.
    Mummification
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary Klingon where
  arbitrary = arbitraryBoundedEnum

instance Hashable Klingon

instance NFData Klingon

instance UnicodeCharacter Klingon where
  toUnicodeChar c
    | c <= GlottalStop = chr (0xf8d0 + ci)
    | c <= Nine = chr (0xf8d6 + ci)
    | otherwise = chr (0xf8d9 + ci)
    where
      ci = fromEnum c
  fromUnicodeChar c
    | c < '\xf8d0' = Nothing
    | c < '\xf8ea' = Just (toEnum (ci - 0xf8d0))
    | c < '\xf8f0' = Nothing
    | c < '\xf8fa' = Just (toEnum (ci - 0xf8d6))
    | c < '\xf8fd' = Nothing
    | c < '\xf900' = Just (toEnum (ci - 0xf8d9))
    | otherwise = Nothing
    where
      ci = ord c
  fromUnicodeChar' c
    | c < '\xf8ea' = toEnum (ci - 0xf8d0)
    | c < '\xf8fa' = toEnum (ci - 0xf8d6)
    | otherwise = toEnum (ci - 0xf8d9)
    where
      ci = ord c
  isInCharRange c = ('\xf8d0' <= c && c <= '\xf8e9') || ('\xf8f0' <= c && c <= '\xf8f9') || ('\xf8fd' <= c && c <= '\xf8ff')

instance UnicodeText Klingon where
  isInTextRange = generateIsInTextRange' @Klingon

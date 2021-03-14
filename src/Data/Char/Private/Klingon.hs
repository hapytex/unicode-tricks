module Data.Char.Private.Klingon (
    -- * Representing Klingon characters
    Klingon(
        A, B, Ch, D, E, Gh, H, I, J, L, M, N, Ng, O, P
      , Q, QUpper, R, S, T, Tlh, U, V, W, Y, GlottalStop
      , Zero, One, Two, Three, Four, Five, Six, Seven
      , Eight, Nine, Comma, FullStop, Mummification
      )
  ) where

-- https://www.evertype.com/standards/csur/klingon.html
-- https://en.wikipedia.org/wiki/Klingon_scripts

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar'), UnicodeText, mapFromEnum, mapToEnum, mapToEnumSafe)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

data Klingon
  = A
  | B
  | Ch
  | D
  | E
  | Gh
  | H
  | I
  | J
  | L
  | M
  | N
  | Ng
  | O
  | P
  | Q
  | QUpper
  | R
  | S
  | T
  | Tlh
  | U
  | V
  | W
  | Y
  | GlottalStop
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Comma
  | FullStop
  | Mummification
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Arbitrary Klingon where
    arbitrary = arbitraryBoundedEnum

{-
instance UnicodeCharacter BallotBox where
    toUnicodeChar = mapFromEnum _ballotOffset
    fromUnicodeChar = mapToEnumSafe _ballotOffset
    fromUnicodeChar' = mapToEnum _ballotOffset

instance UnicodeText BallotBox
-}

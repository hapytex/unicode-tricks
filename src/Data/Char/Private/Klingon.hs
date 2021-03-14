{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Private.Klingon
Description : Support for Klingon, which is sometimes implemented in the /private usage area/.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Some fonts implement the /Klingon script/ characters with the /private usage area/. <https://en.wikipedia.org/wiki/Klingon_scripts Wikipedia>
describes the different fonts and <https://www.evertype.com/standards/csur/klingon.html Evertype> provides a font to typeset Klingon script.
-}

module Data.Char.Private.Klingon (
    -- * Representing Klingon characters
    Klingon(
        A, B, Ch, D, E, Gh, H, I, J, L, M, N, Ng, O, P
      , Q, QUpper, R, S, T, Tlh, U, V, W, Y, GlottalStop
      , Zero, One, Two, Three, Four, Five, Six, Seven
      , Eight, Nine, Comma, FullStop, Mummification
      )
  ) where

import Data.Char(chr, ord)
import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar'), UnicodeText)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- | A datatype to represent the Klingon characters, and their mapping to a Klingon font.
data Klingon
  = A  -- ^ The @a@ character in Klingon script.
  | B  -- ^ The @b@ character in Klingon script.
  | Ch  -- ^ The @ch@ character in Klingon script.
  | D  -- ^ The @D@ character in Klingon script.
  | E  -- ^ The @e@ character in Klingon script.
  | Gh  -- ^ The @gh@ character in Klingon script.
  | H  -- ^ The @H@ character in Klingon script.
  | I  -- ^ The @I@ character in Klingon script.
  | J  -- ^ The @j@ character in Klingon script.
  | L  -- ^ The @l@ character in Klingon script.
  | M  -- ^ The @m@ character in Klingon script.
  | N  -- ^ The @n@ character in Klingon script.
  | Ng  -- ^ The @ng@ character in Klingon script.
  | O  -- ^ The @o@ character in Klingon script.
  | P  -- ^ The @p@ character in Klingon script.
  | Q  -- ^ The @q@ character in Klingon script.
  | QUpper  -- ^ The @Q@ character in Klingon script.
  | R  -- ^ The @r@ character in Klingon script.
  | S  -- ^ The @S@ character in Klingon script.
  | T  -- ^ The @t@ character in Klingon script.
  | Tlh  -- ^ The @tlh@ character in Klingon script.
  | U  -- ^ The @u@ character in Klingon script.
  | V  -- ^ The @v@ character in Klingon script.
  | W  -- ^ The @w@ character in Klingon script.
  | Y  -- ^ The @y@ character in Klingon script.
  | GlottalStop  -- ^ The @ʼ@ character in Klingon script, denoting the /glottal stop/.
  | Zero  -- ^ The @0@ character in Klingon script.
  | One  -- ^ The @1@ character in Klingon script.
  | Two  -- ^ The @2@ character in Klingon script.
  | Three  -- ^ The @3@ character in Klingon script.
  | Four  -- ^ The @4@ character in Klingon script.
  | Five  -- ^ The @5@ character in Klingon script.
  | Six  -- ^ The @6@ character in Klingon script.
  | Seven  -- ^ The @7@ character in Klingon script.
  | Eight  -- ^ The @8@ character in Klingon script.
  | Nine  -- ^ The @9@ character in Klingon script.
  | Comma  -- ^ The /comma/ character in Klingon script, denoted by an /up-turned triangle/.
  | FullStop  -- ^ The /full stop/ character in Klingon script, denoted by a /down-turned triangle/.
  | Mummification  -- ^ The /mummification/ character in Klingon script, also known as the /klingon character for the empire/ or /heart of virtue/.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Arbitrary Klingon where
    arbitrary = arbitraryBoundedEnum

instance UnicodeCharacter Klingon where
    toUnicodeChar c
      | c <= GlottalStop = chr (0xf8d0 + ci)
      | c <= Nine = chr (0xf8d6 + ci)
      | otherwise = chr (0xf8d9 + ci)
      where ci = fromEnum c
    fromUnicodeChar c
      | c < '\xf8d0' = Nothing
      | c < '\xf8ea' = Just (toEnum (ci - 0xf8d0))
      | c < '\xf8f0' = Nothing
      | c < '\xf8fa' = Just (toEnum (ci - 0xf8d6))
      | c < '\xf8fd' = Nothing
      | c < '\xf900' = Just (toEnum (ci - 0xf8d9))
      | otherwise = Nothing
      where ci = ord c
    fromUnicodeChar' c
      | c < '\xf8ea' = toEnum (ci - 0xf8d0)
      | c < '\xf8fa' = toEnum (ci - 0xf8d6)
      | otherwise = toEnum (ci - 0xf8d9)
      where ci = ord c


instance UnicodeText Klingon

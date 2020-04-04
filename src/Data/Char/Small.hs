{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Small
Description : A module used to render subscript and superscript in Unicode.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

One can make use of a <https://www.unicode.org/charts/PDF/U2070.pdf block of Unicode characters> to /emulate/ subscript and superscript. Note that the subscript and superscript will be
aligned with the /baseline/ and the /cap line/ respectively, and is thus not equivalent to @<sub>...</sub>@ and @<sup>...</sup>@ in HTML. Furthermore only a small subset of characters
is supported.

This module allows one to map certain characters to their subscript and superscript counterpart, and furthermore makes it more convenient to transform a number (both positive and negative)
to a 'Text' that specifies this number in subscript and superscript.
-}

module Data.Char.Small (
  -- * Convert characters to their subscript and superscript counterpart
    toSub, toSup
  -- * Numbers as subscript and superscript.
  , asSub, asSup
  ) where

import Data.Char(chr, isDigit, ord)
import Data.Text(Text, cons, snoc, singleton)

-- | Convert a set of characters to their superscript counterpart, given that
-- characters exists.
toSup
    :: Char  -- ^ The given character to convert to its superscript counterpart.
    -> Maybe Char -- ^ A character wrapped in a 'Just' given the counterpart exists, 'Nothing' otherwise.
toSup 'i' = Just '\x2071'
toSup '+' = Just '\x207a'
toSup '-' = Just '\x207b'
toSup '\x2212' = Just '\x207b'
toSup '=' = Just '\x207c'
toSup '(' = Just '\x207d'
toSup ')' = Just '\x207e'
toSup 'n' = Just '\x207f'
toSup c | isDigit c = Just (_digitToSub (ord c - ord '0'))
        | otherwise = Nothing

-- | Convert a set of characters to their subscript counterpart, given that
-- characters exists.
toSub
    :: Char  -- ^ The given character to convert to its subscript counterpart.
    -> Maybe Char -- ^ A character wrapped in a 'Just' given the counterpart exists, 'Nothing' otherwise.
toSub '+' = Just '\x208a'
toSub '-' = Just '\x208b'
toSub '\x2212' = Just '\x208b'
toSub '=' = Just '\x208c'
toSub '(' = Just '\x208d'
toSub ')' = Just '\x208e'
toSub 'a' = Just '\x2090'
toSub 'e' = Just '\x2091'
toSub 'o' = Just '\x2092'
toSub 'x' = Just '\x2093'
toSub '\x259' = Just '\x2094'
toSub 'h' = Just '\x2095'
toSub 'k' = Just '\x2095'
toSub 'l' = Just '\x2095'
toSub 'm' = Just '\x2095'
toSub 'n' = Just '\x2095'
toSub 'p' = Just '\x2095'
toSub 's' = Just '\x2095'
toSub 't' = Just '\x2095'
toSub c | isDigit c = Just (_digitToSub (ord c - ord '0'))
        | otherwise = Nothing

_value :: Integral i => (Int -> Char) -> i -> Text
_value f = go
    where f' = f . fromIntegral
          go n | n <= 9 = singleton (f' n)
               | otherwise = snoc (go q) (f' r)
               where (q,r) = quotRem n 10

_prefixSign :: Integral i => Char -> (Int -> Char) -> i -> Text
_prefixSign c f v
    | v < 0 = cons c (f' (-v))
    | otherwise = f' v
    where f' = _value f

-- | Convert a number (positive or negative) to a 'Text' that specifies that
-- number in superscript characters.
asSup :: Integral i
    => i -- ^ The number to convert.
    -> Text -- ^ A 'Text' value that contains the number as a sequence of superscript characters.
asSup = _prefixSign '\x207b' _digitToSup

-- | Convert a number (positive or negative) to a 'Text' that specifies that
-- number in subscript characters.
asSub :: Integral i
    => i -- ^ The number to convert.
    -> Text -- ^ A 'Text' value that contains the number as a sequence of subscript characters.
asSub = _prefixSign '\x208b' _digitToSub

_digitToSub :: Int -> Char
_digitToSub = chr . (8320+)

_digitToSup :: Int -> Char
_digitToSup 0 = '\x2070'
_digitToSup 1 = '\xb9'
_digitToSup n | n <= 3 = chr (176+n)
              | otherwise = chr (8304+n)

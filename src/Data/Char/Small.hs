{-# LANGUAGE CPP, Safe #-}

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
  -- * Convert superscript and subscript back their normal character
  , fromSubSup
  -- * Numbers as subscript and superscript.
  , asSub, asSub', asSubPlus
  , asSup, asSup', asSupPlus
  -- * Ratio formatting
  , ratioToUnicode, ratioToUnicode', ratioPartsToUnicode, ratioPartsToUnicode'
  ) where

import Data.Bits((.&.), (.|.))
import Data.Char(chr, isDigit, ord)
import Data.Char.Core(PlusStyle(WithPlus, WithoutPlus), positionalNumberSystem10)
import Data.Default(Default(def))
import Data.Ratio(Ratio, denominator, numerator)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif

import Data.Text(Text, cons, snoc, singleton)

-- | Convert a set of characters to their superscript counterpart, given that
-- characters exists.
toSup
    :: Char  -- ^ The given character to convert to its superscript counterpart.
    -> Maybe Char  -- ^ A character wrapped in a 'Just' given the counterpart exists, 'Nothing' otherwise.
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
    -> Maybe Char  -- ^ A character wrapped in a 'Just' given the counterpart exists, 'Nothing' otherwise.
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
toSub 'k' = Just '\x2096'
toSub 'l' = Just '\x2097'
toSub 'm' = Just '\x2098'
toSub 'n' = Just '\x2099'
toSub 'p' = Just '\x209a'
toSub 's' = Just '\x209b'
toSub 't' = Just '\x209c'
toSub c | isDigit c = Just (_digitToSub (ord c - ord '0'))
        | otherwise = Nothing

_fromSubSup :: Int -> Char
_fromSubSup 0xa = '+'
_fromSubSup 0xb = '-'
_fromSubSup 0xc = '='
_fromSubSup 0xd = '('
_fromSubSup 0xe = ')'
_fromSubSup _ = undefined

fromSubSup :: Char -> Char
fromSubSup '\x2070' = '0'
fromSubSup '\xb2' = '2'
fromSubSup '\xb3' = '3'
fromSubSup '\xb9' = '1'
fromSubSup '\x2071' = 'i'
fromSubSup '\x207f' = 'n'
fromSubSup '\x2090' = 'a'
fromSubSup '\x2091' = 'e'
fromSubSup '\x2092' = 'o'
fromSubSup '\x2093' = 'x'
fromSubSup '\x2094' = '\x259'
fromSubSup '\x2095' = 'h'
fromSubSup '\x2096' = 'k'
fromSubSup '\x2097' = 'l'
fromSubSup '\x2098' = 'm'
fromSubSup '\x2099' = 'n'
fromSubSup '\x209a' = 'p'
fromSubSup '\x209b' = 's'
fromSubSup '\x209c' = 't'
fromSubSup x
  | '\x207a' <= x && x<= '\x208e' && 0x0a <= m && m <= 0x0e = _fromSubSup m
  | '\x2074' <= x && x <= '\x2089' = chr (0x30 .|. (ord x .&. 0xf))
  | otherwise = x
  where m = ord x .&. 0xf

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

_prefixSignPlus :: Integral i => Char -> Char -> (Int -> Char) -> i -> Text
_prefixSignPlus cp cn f v
  | v < 0 = c' cn (-v)
  | otherwise = c' cp v
  where c' = (. _value f) . cons

-- | Converting the given numerator and denominator to a fraction
-- where the numerator is written in superscript, and the denominator
-- in subscript. If the denominator is negative, the item is rendered
-- with a minus at the numerator part.
ratioPartsToUnicode :: (Integral i, Integral j)
  => PlusStyle -- ^ the given plus style that will be applied to the numerator.
  -> i  -- ^ The given numerator.
  -> j  -- ^ The given denominator.
  -> Text  -- ^ A 'Text' object that presents the fraction with superscript and subscript.
ratioPartsToUnicode ps num den
  | den < 0 = ratioPartsToUnicode ps (-num) (-den)
  | otherwise = asSup ps num <> cons '\x2044' (asSub' den)


-- | Converting the given numerator and denominator to a fraction
-- where the numerator is written in superscript, and the denominator
-- in subscript. If the denominator is negative, the item is rendered
-- with a minus at the numerator part.
ratioPartsToUnicode' :: (Integral i, Integral j)
  => i  -- ^ The given numerator.
  -> j  -- ^ The given denominator.
  -> Text  -- ^ A 'Text' object that presents the fraction with superscript and subscript.
ratioPartsToUnicode' = ratioPartsToUnicode WithoutPlus

-- | Convert the given 'Ratio' object to a sequence of characters with the
-- numerator in superscript and the denominator in subscript. The given
-- 'PlusStyle' is applied to the numerator.
ratioToUnicode :: Integral i
  => PlusStyle  -- ^ The given 'PlusStyle' to use.
  -> Ratio i  -- ^ The given 'Ratio' object to convert to a 'Text'.
  -> Text  -- ^ A 'Text' object that denotes the given 'Ratio' making use of superscript and subscript.
ratioToUnicode ps dn = ratioPartsToUnicode ps (numerator dn) (denominator dn)

-- | Format a given 'Ratio' object to a 'Text' value that formats the ratio with
-- superscript and subscript using the 'Default' 'PlusStyle'.
ratioToUnicode' :: Integral i
    => Ratio i  -- ^ The given 'Ratio' value to format.
    -> Text  -- ^ The 'Text' block that contains a textual representation of the 'Ratio'.
ratioToUnicode' = ratioToUnicode def

-- | Convert a number (positive or negative) to a 'Text' object that denotes
-- that number in superscript characters.
asSup :: Integral i
  => PlusStyle  -- ^ The given 'PlusStyle' to use.
  -> i  -- ^ The given number to convert.
  -> Text  -- ^ A 'Text' value that denotes the number as a sequence of superscript characters.
asSup = positionalNumberSystem10 _digitToSup '\x207a' '\x207b'

-- | Convert a number (positive or negative) to a 'Text' object that denotes that
-- number in superscript characters.
asSup' :: Integral i
    => i  -- ^ The number to convert.
    -> Text  -- ^ A 'Text' value that contains the number as a sequence of superscript characters.
asSup' = asSup WithoutPlus

-- | Convert a number (positive or negative) to a 'Text' that specifies that
-- number in superscript characters. For positive characters, the superscript
-- contains a plus character (@⁺@).
asSupPlus :: Integral i
    => i  -- ^ The number to convert.
    -> Text  -- ^ A 'Text' value that contains the number as a sequence of superscript characters.
asSupPlus = asSup WithPlus -- _prefixSignPlus '\x207a' '\x207b' _digitToSup

-- | Convert a number (positive or negative) to a 'Text' object that denotes
-- that number in subscript characters.
asSub :: Integral i
  => PlusStyle  -- ^ The given 'PlusStyle' to use.
  -> i  -- ^ The given number to convert.
  -> Text  -- ^ A 'Text' value that denotes the number as a sequence of subscript characters.
asSub = positionalNumberSystem10 _digitToSub '\x208a' '\x208b'

-- | Convert a number (positive or negative) to a 'Text' that specifies that
-- number in subscript characters.
asSub' :: Integral i
    => i  -- ^ The number to convert.
    -> Text  -- ^ A 'Text' value that contains the number as a sequence of subscript characters.
asSub' = asSub WithoutPlus

-- | Convert a number (positive or negative) to a 'Text' that specifies that
-- number in subscript characters. For positive characters, the subscript
-- contains a plus character (@₊@).
asSubPlus :: Integral i
    => i  -- ^ The number to convert.
    -> Text  -- ^ A 'Text' value that contains the number as a sequence of subscript characters.
asSubPlus = asSub WithPlus

_digitToSub :: Int -> Char
_digitToSub = chr . (8320+)

_digitToSup :: Int -> Char
_digitToSup 0 = '\x2070'
_digitToSup 1 = '\xb9'
_digitToSup n | n <= 3 = chr (176+n)
              | otherwise = chr (8304+n)

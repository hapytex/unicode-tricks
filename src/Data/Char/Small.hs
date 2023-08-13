{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      : Data.Char.Small
-- Description : A module used to render subscript and superscript in Unicode.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- One can make use of a <https://www.unicode.org/charts/PDF/U2070.pdf block of Unicode characters> to /emulate/ subscript and superscript. Note that the subscript and superscript will be
-- aligned with the /baseline/ and the /cap line/ respectively, and is thus not equivalent to @<sub>...</sub>@ and @<sup>...</sup>@ in HTML. Furthermore only a small subset of characters
-- is supported.
--
-- This module allows one to map certain characters to their subscript and superscript counterpart, and furthermore makes it more convenient to transform a number (both positive and negative)
-- to a 'Text' that specifies this number in subscript and superscript.
module Data.Char.Small
  ( -- * Convert characters to their subscript and superscript counterpart
    toSub,
    toSup,

    -- * Convert superscript and subscript back their normal character
    fromSubSup,

    -- * Numbers as subscript and superscript.
    asSub,
    asSub',
    asSubPlus,
    asSup,
    asSup',
    asSupPlus,

    -- * Ratio formatting
    ratioToUnicode,
    ratioToUnicode',
    ratioPartsToUnicode,
    ratioPartsToUnicode',

    -- * Ratio parsing
    unicodeToRatio,
    unicodeToRatioParts,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.Char (chr, isDigit, ord)
import Data.Char.Core (PlusStyle (WithPlus, WithoutPlus), positionalNumberSystem10)
import Data.Default.Class (Default (def))
import Data.Ratio (Ratio, denominator, numerator, (%))
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif

import Data.Text (Text, cons, singleton, snoc, unpack)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | Convert a set of characters to their superscript counterpart, given that
-- characters exists.
toSup ::
  -- | The given character to convert to its superscript counterpart.
  Char ->
  -- | A character wrapped in a 'Just' given the counterpart exists, 'Nothing' otherwise.
  Maybe Char
toSup 'i' = Just '\x2071'
toSup '+' = Just '\x207a'
toSup '-' = Just '\x207b'
toSup '\x2212' = Just '\x207b'
toSup '=' = Just '\x207c'
toSup '(' = Just '\x207d'
toSup ')' = Just '\x207e'
toSup 'n' = Just '\x207f'
toSup 'h' = Just '\x02b0'
toSup 'ɦ' = Just '\x02b1'
toSup 'j' = Just '\x02b2'
toSup 'r' = Just '\x02b3'
toSup 'ɹ' = Just '\x02b4'
toSup 'ɻ' = Just '\x02b5'
toSup 'ʁ' = Just '\x02b6'
toSup 'w' = Just '\x02b7'
toSup 'y' = Just '\x02b8'
toSup c
  | isDigit c = Just (_digitToSub (ord c - ord '0'))
  | otherwise = Nothing

-- | Convert a set of characters to their subscript counterpart, given that
-- characters exists.
toSub ::
  -- | The given character to convert to its subscript counterpart.
  Char ->
  -- | A character wrapped in a 'Just' given the counterpart exists, 'Nothing' otherwise.
  Maybe Char
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
toSub c
  | isDigit c = Just (_digitToSub (ord c - ord '0'))
  | otherwise = Nothing

_fromSubSup :: Int -> Char
_fromSubSup 0xa = '+'
_fromSubSup 0xb = '-'
_fromSubSup 0xc = '='
_fromSubSup 0xd = '('
_fromSubSup 0xe = ')'
_fromSubSup _ = error "Should not happen!"

-- | Convert subscripts and superscripts back to the original counterpart, so @'⁵'@ back to @'5'@. For non-subscript or -superscript
-- characters, it returns the original character.
fromSubSup ::
  -- | A character to un-subscript or un-superscript, for example @'⁵'@.
  Char ->
  -- | The corresponding original character, for example @'5'@.
  Char
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
  | '\x207a' <= x && x <= '\x208e' && 0x0a <= m && m <= 0x0e = _fromSubSup m
  | '\x2074' <= x && x <= '\x2089' = chr (0x30 .|. (ord x .&. 0xf))
  | otherwise = x
  where
    m = ord x .&. 0xf

_value :: Integral i => (Int -> Char) -> i -> Text
_value f = go
  where
    f' = f . fromIntegral
    go n
      | n <= 9 = singleton (f' n)
      | otherwise = snoc (go q) (f' r)
      where
        (q, r) = quotRem n 10

_prefixSign :: Integral i => Char -> (Int -> Char) -> i -> Text
_prefixSign c f v
  | v < 0 = cons c (f' (-v))
  | otherwise = f' v
  where
    f' = _value f

_prefixSignPlus :: Integral i => Char -> Char -> (Int -> Char) -> i -> Text
_prefixSignPlus cp cn f v
  | v < 0 = c' cn (-v)
  | otherwise = c' cp v
  where
    c' = (. _value f) . cons

-- | Converting the given numerator and denominator to a fraction
-- where the numerator is written in superscript, and the denominator
-- in subscript. If the denominator is negative, the item is rendered
-- with a minus at the numerator part.
ratioPartsToUnicode ::
  (Integral i, Integral j) =>
  -- | the given plus style that will be applied to the numerator.
  PlusStyle ->
  -- | The given numerator.
  i ->
  -- | The given denominator.
  j ->
  -- | A 'Text' object that presents the fraction with superscript and subscript.
  Text
ratioPartsToUnicode ps num den
  | den < 0 = ratioPartsToUnicode ps (-num) (-den)
  | otherwise = asSup ps num <> cons '\x2044' (asSub' den)

-- | Converting the given numerator and denominator to a fraction
-- where the numerator is written in superscript, and the denominator
-- in subscript. If the denominator is negative, the item is rendered
-- with a minus at the numerator part.
ratioPartsToUnicode' ::
  (Integral i, Integral j) =>
  -- | The given numerator.
  i ->
  -- | The given denominator.
  j ->
  -- | A 'Text' object that presents the fraction with superscript and subscript.
  Text
ratioPartsToUnicode' = ratioPartsToUnicode def

-- | Try to convert the given text that contains a fraction to the numerator and denominator. This does *not* take /vulgar fractions/
-- into account. You can process these with 'Dat.Char.Number.VulgarFraction.fromVulgarFallback'.
unicodeToRatioParts ::
  (Read i, Read j) =>
  -- | The 'Text' we try to decode.
  Text ->
  -- | A 2-tuple with the numerator and denominator wrapped in a 'Just' if the fraction can be parsed, 'Nothing' otherwise.
  Maybe (i, j)
unicodeToRatioParts t = (,) <$> _parseInt (unpack n) <*> _parseInt (drop 1 (unpack d))
  where
    ~(n, d) = T.break _isFrac (T.map fromSubSup t)

-- | Try to convert the given text that contains a fraction to a 'Ratio'. This does *not* take /vulgar fractions/
-- into account. You can process these with 'Dat.Char.Number.VulgarFraction.fromVulgarFallbackToRatio'.
unicodeToRatio ::
  (Integral i, Read i) =>
  -- | The 'Text' we try to decode.
  Text ->
  -- | The fraction wrapped in a 'Just'; 'Nothing' if the fraction can not be parsed.
  Maybe (Ratio i)
unicodeToRatio = fmap (uncurry (%)) . unicodeToRatioParts

-- | Convert the given 'Ratio' object to a sequence of characters with the
-- numerator in superscript and the denominator in subscript. The given
-- 'PlusStyle' is applied to the numerator.
ratioToUnicode ::
  Integral i =>
  -- | The given 'PlusStyle' to use.
  PlusStyle ->
  -- | The given 'Ratio' object to convert to a 'Text'.
  Ratio i ->
  -- | A 'Text' object that denotes the given 'Ratio' making use of superscript and subscript.
  Text
ratioToUnicode ps dn = ratioPartsToUnicode ps (numerator dn) (denominator dn)

-- | Format a given 'Ratio' object to a 'Text' value that formats the ratio with
-- superscript and subscript using the 'Default' 'PlusStyle'.
ratioToUnicode' ::
  Integral i =>
  -- | The given 'Ratio' value to format.
  Ratio i ->
  -- | The 'Text' block that contains a textual representation of the 'Ratio'.
  Text
ratioToUnicode' = ratioToUnicode def

-- | Convert a number (positive or negative) to a 'Text' object that denotes
-- that number in superscript characters.
asSup ::
  Integral i =>
  -- | The given 'PlusStyle' to use.
  PlusStyle ->
  -- | The given number to convert.
  i ->
  -- | A 'Text' value that denotes the number as a sequence of superscript characters.
  Text
asSup = positionalNumberSystem10 _digitToSup '\x207a' '\x207b'

-- | Convert a number (positive or negative) to a 'Text' object that denotes that
-- number in superscript characters.
asSup' ::
  Integral i =>
  -- | The number to convert.
  i ->
  -- | A 'Text' value that contains the number as a sequence of superscript characters.
  Text
asSup' = asSup WithoutPlus

-- | Convert a number (positive or negative) to a 'Text' that specifies that
-- number in superscript characters. For positive characters, the superscript
-- contains a plus character (@⁺@).
asSupPlus ::
  Integral i =>
  -- | The number to convert.
  i ->
  -- | A 'Text' value that contains the number as a sequence of superscript characters.
  Text
asSupPlus = asSup WithPlus -- _prefixSignPlus '\x207a' '\x207b' _digitToSup

-- | Convert a number (positive or negative) to a 'Text' object that denotes
-- that number in subscript characters.
asSub ::
  Integral i =>
  -- | The given 'PlusStyle' to use.
  PlusStyle ->
  -- | The given number to convert.
  i ->
  -- | A 'Text' value that denotes the number as a sequence of subscript characters.
  Text
asSub = positionalNumberSystem10 _digitToSub '\x208a' '\x208b'

-- | Convert a number (positive or negative) to a 'Text' that specifies that
-- number in subscript characters.
asSub' ::
  Integral i =>
  -- | The number to convert.
  i ->
  -- | A 'Text' value that contains the number as a sequence of subscript characters.
  Text
asSub' = asSub WithoutPlus

-- | Convert a number (positive or negative) to a 'Text' that specifies that
-- number in subscript characters. For positive characters, the subscript
-- contains a plus character (@₊@).
asSubPlus ::
  Integral i =>
  -- | The number to convert.
  i ->
  -- | A 'Text' value that contains the number as a sequence of subscript characters.
  Text
asSubPlus = asSub WithPlus

_digitToSub :: Int -> Char
_digitToSub = chr . (8320 +)

_digitToSup :: Int -> Char
_digitToSup 0 = '\x2070'
_digitToSup 1 = '\xb9'
_digitToSup n
  | n <= 3 = chr (176 + n)
  | otherwise = chr (8304 + n)

_parseInt :: Read i => String -> Maybe i
_parseInt ('+' : d) = readMaybe d
_parseInt d = readMaybe d

_isFrac :: Char -> Bool
_isFrac '/' = True
_isFrac '\x2044' = True
_isFrac _ = False

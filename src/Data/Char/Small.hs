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
toSup 'A' = Just '\x1d2c'
toSup 'Æ' = Just '\x1d2d'
toSup 'B' = Just '\x1d2e'
toSup 'D' = Just '\x1d30'
toSup 'E' = Just '\x1d31'
toSup 'Ǝ' = Just '\x1d32'
toSup 'G' = Just '\x1d33'
toSup 'H' = Just '\x1d34'
toSup 'I' = Just '\x1d35'
toSup 'J' = Just '\x1d36'
toSup 'K' = Just '\x1d37'
toSup 'L' = Just '\x1d38'
toSup 'M' = Just '\x1d39'
toSup 'N' = Just '\x1d3a'
toSup 'O' = Just '\x1d3c'
toSup 'Ȣ' = Just '\x1d3d'
toSup 'P' = Just '\x1d3e'
toSup 'R' = Just '\x1d3f'
toSup 'T' = Just '\x1d40'
toSup 'U' = Just '\x1d41'
toSup 'W' = Just '\x1d42'
toSup 'a' = Just '\x1d43'
toSup 'ɐ' = Just '\x1d44'
toSup 'ɑ' = Just '\x1d45'
toSup 'ᴂ' = Just '\x1d46'
toSup 'b' = Just '\x1d47'
toSup 'd' = Just '\x1d48'
toSup 'e' = Just '\x1d49'
toSup 'ə' = Just '\x1d4a'
toSup 'ɛ' = Just '\x1d4b'
toSup 'ɜ' = Just '\x1d4c'
toSup 'g' = Just '\x1d4d'
toSup 'k' = Just '\x1d4f'
toSup 'm' = Just '\x1d50'
toSup 'ŋ' = Just '\x1d51'
toSup 'o' = Just '\x1d52'
toSup 'ɔ' = Just '\x1d53'
toSup 'ᴖ' = Just '\x1d54'
toSup 'ᴗ' = Just '\x1d55'
toSup 'p' = Just '\x1d56'
toSup 't' = Just '\x1d57'
toSup 'u' = Just '\x1d58'
toSup 'ᴝ' = Just '\x1d59'
toSup 'ɯ' = Just '\x1d5a'
toSup 'v' = Just '\x1d5b'
toSup 'ᴥ' = Just '\x1d5c'
toSup 'н' = Just '\x1d78'
toSup 'β' = Just '\x1d5d'
toSup 'γ' = Just '\x1d5e'
toSup 'δ' = Just '\x1d5f'
toSup 'φ' = Just '\x1d60'
toSup 'χ' = Just '\x1d61'
toSup 'ɣ' = Just '\x02e0'
toSup 'l' = Just '\x02e1'
toSup 's' = Just '\x02e2'
toSup 'x' = Just '\x02e3'
toSup 'ʕ' = Just '\x02e4'
toSup 'ნ' = Just '\x10fc'
toSup 'ɒ' = Just '\x1d9b'
toSup 'c' = Just '\x1d9c'
toSup 'ɕ' = Just '\x1d9d'
toSup 'ð' = Just '\x1d9e'
-- toSup 'ɜ' = Just '\x1d9f'  already \1d4c
toSup 'f' = Just '\x1da0'
toSup 'ɟ' = Just '\x1da1'
toSup 'ɡ' = Just '\x1da2'
toSup 'ɥ' = Just '\x1da3'
toSup 'ɨ' = Just '\x1da4'
toSup 'ɩ' = Just '\x1da5'
toSup 'ɪ' = Just '\x1da6'
toSup 'ᵻ' = Just '\x1da7'
toSup 'ʝ' = Just '\x1da8'
toSup 'ɭ' = Just '\x1da9'
toSup 'ᶅ' = Just '\x1daa'
toSup 'ʟ' = Just '\x1dab'
toSup 'ɱ' = Just '\x1dac'
toSup 'ɰ' = Just '\x1dad'
toSup 'ɲ' = Just '\x1dae'
toSup 'ɳ' = Just '\x1daf'
toSup 'ɴ' = Just '\x1db0'
toSup 'ɵ' = Just '\x1db1'
toSup 'ɸ' = Just '\x1db2'
toSup 'ʂ' = Just '\x1db3'
toSup 'ʃ' = Just '\x1db4'
toSup 'ƫ' = Just '\x1db5'
toSup 'ʉ' = Just '\x1db6'
toSup 'ʊ' = Just '\x1db7'
toSup 'ᴜ' = Just '\x1db8'
toSup 'ʋ' = Just '\x1db9'
toSup 'ʌ' = Just '\x1dba'
toSup 'z' = Just '\x1dbb'
toSup 'ʐ' = Just '\x1dbc'
toSup 'ʑ' = Just '\x1dbd'
toSup 'ʒ' = Just '\x1dbe'
toSup 'θ' = Just '\x1dbf'
toSup 'V' = Just '\x2c7d'
toSup 'ⵡ' = Just '\x2d7f'
toSup '一' = Just '\x3192'
toSup '二' = Just '\x3193'
toSup '三' = Just '\x3194'
toSup '四' = Just '\x3195'
toSup '上' = Just '\x3196'
toSup '中' = Just '\x3197'
toSup '下' = Just '\x3198'
toSup '甲' = Just '\x3199'
toSup '乙' = Just '\x319a'
toSup '丙' = Just '\x319b'
toSup '丁' = Just '\x319c'
toSup '天' = Just '\x319d'
toSup '地' = Just '\x319e'
toSup '人' = Just '\x319f'
toSup 'ъ' = Just '\xa69c'
toSup 'ь' = Just '\xa69d'
toSup 'ꝯ' = Just '\xa770'
toSup 'C' = Just '\xa7f2'
toSup 'F' = Just '\xa7f3'
toSup 'Q' = Just '\xa7f4'
toSup 'Ħ' = Just '\xa7f8'
toSup 'œ' = Just '\xa7f9'
toSup 'ꜧ' = Just '\xab5c'
toSup 'ꬷ' = Just '\xab5d'
toSup 'ɫ' = Just '\xab5e'
toSup 'ꭒ' = Just '\xab5f'
toSup 'ʍ' = Just '\xab69'
toSup 'ː' = Just '\x10781'
toSup 'ˑ' = Just '\x10782'
toSup 'æ' = Just '\x10783'
toSup 'ʙ' = Just '\x10784'
toSup 'ɓ' = Just '\x10785'
toSup 'ʣ' = Just '\x10787'
toSup '\xab66' = Just '\x10788'
toSup 'ʥ' = Just '\x10789'
toSup 'ʤ' = Just '\x1078a'
toSup 'ɖ' = Just '\x1078b'
toSup 'ɗ' = Just '\x1078c'
toSup 'ᶑ' = Just '\x1078d'
toSup 'ɘ' = Just '\x1078e'
toSup 'ɞ' = Just '\x1078f'
toSup 'ʩ' = Just '\x10790'
toSup 'ɤ' = Just '\x10791'
toSup 'ɢ' = Just '\x10792'
toSup 'ɠ' = Just '\x10793'
toSup 'ʛ' = Just '\x10794'
toSup 'ħ' = Just '\x10795'
toSup 'ʜ' = Just '\x10796'
toSup 'ɧ' = Just '\x10797'
toSup 'ʄ' = Just '\x10798'
toSup 'ʪ' = Just '\x10799'
toSup 'ʫ' = Just '\x1079a'
toSup 'ɬ' = Just '\x1079b'
toSup '\x1df04' = Just '\x1079c'
toSup 'ꞎ' = Just '\x1079d'
toSup 'ɮ' = Just '\x1079e'
toSup '\x1df05' = Just '\x1079f'
toSup 'ʎ' = Just '\x107a0'
toSup '\x1df06' = Just '\x107a1'
toSup 'ø' = Just '\x107a2'
toSup 'ɶ' = Just '\x107a3'
toSup 'ɷ' = Just '\x107a4'
toSup 'q' = Just '\x107a5'
toSup 'ɺ' = Just '\x107a6'
toSup '\x1df08' = Just '\x107a7'
toSup 'ɽ' = Just '\x107a8'
toSup 'ɾ' = Just '\x107a9'
toSup 'ʀ' = Just '\x107aa'
toSup 'ʨ' = Just '\x107ab'
toSup 'ʦ' = Just '\x107ac'
toSup '\xab67' = Just '\x107ad'
toSup 'ʧ' = Just '\x107ae'
toSup 'ʈ' = Just '\x107af'
toSup 'ⱱ' = Just '\x107b0'
toSup 'ʏ' = Just '\x107b2'
toSup 'ʡ' = Just '\x107b3'
toSup 'ʢ' = Just '\x107b4'
toSup 'ʘ' = Just '\x107b5'
toSup 'ǀ' = Just '\x107b6'
toSup 'ǁ' = Just '\x107b7'
toSup 'ǂ' = Just '\x107b8'
toSup '\x1df0a' = Just '\x107b9'
toSup '\x1df1e' = Just '\x107ba'
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
toSub 'j' = Just '\x2c7c'
toSub 'k' = Just '\x2096'
toSub 'l' = Just '\x2097'
toSub 'm' = Just '\x2098'
toSub 'n' = Just '\x2099'
toSub 'p' = Just '\x209a'
toSub 's' = Just '\x209b'
toSub 't' = Just '\x209c'
toSub 'i' = Just '\x1d62'
toSub 'r' = Just '\x1d63'
toSub 'u' = Just '\x1d64'
toSub 'v' = Just '\x1d65'
toSub 'β' = Just '\x1d66'
toSub 'γ' = Just '\x1d67'
toSub 'ρ' = Just '\x1d68'
toSub 'φ' = Just '\x1d69'
toSub 'χ' = Just '\x1d6a'
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

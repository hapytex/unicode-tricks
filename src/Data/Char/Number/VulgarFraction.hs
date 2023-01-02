{-# LANGUAGE Safe, TupleSections #-}

{-|
Module      : Data.Char.Number.VulgarFraction
Description : A module used to render vulgar fractions with Unicode.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Uncode has two blocks where vulgar fractions are defined: <https://www.unicode.org/charts/PDF/U0080.pdf C1 controls and latin supplement 1> and
<https://www.unicode.org/charts/PDF/U2150.pdf Number forms>. These are fractions that are commenly used.

The module exports function 'toVulgar' and 'ratioToVulgar' to convert the ratio to a 'Char' with that fraction if that exists. The
functon 'ratioToVulgarFallback' and 'ratioToVulgarFallback' are used to try to find a vulgar fraction character, and if that fails,
it prints the fraction with the help of the 'Data.Char.Small' module.
-}


module Data.Char.Number.VulgarFraction (
    -- * Render to a vulgar fraction
    ratioToVulgar, toVulgar
    -- * Try to parse a vulgar fraction
  , fromVulgar, fromVulgarToRatio
    -- * Render to a vulgar fraction, with a fallback to using small characters
  , ratioToVulgarFallback, toVulgarFallback
    -- * Convert 'Text' to a vulgar fraction or fallback on a 'Text' that contains a numerator and denominator as a sequence of characters
  , fromVulgarFallback, fromVulgarFallbackToRatio
  ) where

import Control.Applicative((<|>))

import Data.Ratio(Ratio, numerator, denominator, (%))
import Data.Text(Text, cons, singleton, unpack)

import Text.Read(readMaybe)

import Data.Char.Small(asSub', fromSubSup, ratioPartsToUnicode', unicodeToRatioParts)

-- | Convert the given 'Ratio' item to a vulgar fraction character, if such character exists; 'Nothing' otherwise.
ratioToVulgar :: Integral i
  => Ratio i  -- ^ The 'Ratio' for which we try to find the corresponding 'Char'acter.
  -> Maybe Char -- ^ The corresponding 'Char'acter wrapped in a 'Just' if such character exists; 'Nothing' otherwise.
ratioToVulgar r = toVulgar (numerator r) (denominator r)

-- | Convert the given 'Ratio' to a singleton 'Text' with the vulgar fraction character,
-- if such character exists; it will make ue of the 'ratioPartsToUnicode'' to generate a 'Text'
-- object (with multiple 'Char'acters) that looks like a fraction.
ratioToVulgarFallback :: Integral i
  => Ratio i  -- ^ The given 'Ratio' to convert.
  -> Text  -- ^ A 'Text' object with a single 'Char'acter if a vulgar fraction character exists; otherwise a 'Text' object created by 'ratioPartsToUnicode''.
ratioToVulgarFallback nd = toVulgarFallback (numerator nd) (denominator nd)

-- | Convert the given /numerator/ den /denominator/ to a vulgar fraction character, if such character exists; 'Nothing' otherwise.
toVulgar :: (Integral i, Integral j)
  => i  -- ^ The given numerator.
  -> j  -- ^ The given denominator.
  -> Maybe Char -- ^ The corresponding 'Char'acter wrapped in a 'Just' if such character exists; 'Nothing' otherwise.
toVulgar 1 4 = Just '\x00bc'
toVulgar 1 2 = Just '\x00bd'
toVulgar 3 4 = Just '\x00be'
toVulgar 1 7 = Just '\x2150'
toVulgar 1 9 = Just '\x2151'
toVulgar 1 10 = Just '\x2152'
toVulgar 1 3 = Just '\x2153'
toVulgar 2 3 = Just '\x2154'
toVulgar 1 5 = Just '\x2155'
toVulgar 2 5 = Just '\x2156'
toVulgar 3 5 = Just '\x2157'
toVulgar 4 5 = Just '\x2158'
toVulgar 1 6 = Just '\x2159'
toVulgar 5 6 = Just '\x215a'
toVulgar 1 8 = Just '\x215b'
toVulgar 3 8 = Just '\x215c'
toVulgar 5 8 = Just '\x215d'
toVulgar 7 8 = Just '\x215e'
toVulgar 0 3 = Just '\x2189'  -- used in baseball
toVulgar _ _ = Nothing

-- | Convert the given numerator and denominator to a singleton 'Text' with the vulgar fraction character,
-- if such character exists; it will make ue of the 'ratioPartsToUnicode'' to generate a 'Text'
-- object (with multiple 'Char'acters) that looks like a fraction.
toVulgarFallback :: (Integral i, Integral j)
  => i  -- ^ The given /numerator/.
  -> j  -- ^ The given /denominator/.
  -> Text  -- ^ A 'Text' object with a single 'Char'acter if a vulgar fraction character exists; otherwise a 'Text' object created by 'ratioPartsToUnicode''.
toVulgarFallback i j = maybe (go i j) singleton (toVulgar i j)
  where go 1 d | d > 0 = cons '\x215f' ( asSub' d)
        go n d = ratioPartsToUnicode' n d

-- | Try to convert a given 'Char', if it is a /vulgar fraction/, to a 2-tuple with the numerator and denominator. Returns 'Nothing' if the 'Char'
-- is not a vulgar fraction character.
fromVulgar :: (Integral i, Integral j)
  => Char  -- ^ The character to decode.
  -> Maybe (i, j)  -- ^ The numerator and denominator wrapped in a 'Just' if the character is a vulgar fraction, 'Nothing' otherwise.
fromVulgar '\x00bc' = Just (1, 4)
fromVulgar '\x00bd' = Just (1, 2)
fromVulgar '\x00be' = Just (3, 4)
fromVulgar '\x2150' = Just (1, 7)
fromVulgar '\x2151' = Just (1, 9)
fromVulgar '\x2152' = Just (1, 10)
fromVulgar '\x2153' = Just (1, 3)
fromVulgar '\x2154' = Just (2, 3)
fromVulgar '\x2155' = Just (1, 5)
fromVulgar '\x2156' = Just (2, 5)
fromVulgar '\x2157' = Just (3, 5)
fromVulgar '\x2158' = Just (4, 5)
fromVulgar '\x2159' = Just (1, 6)
fromVulgar '\x215a' = Just (5, 6)
fromVulgar '\x215b' = Just (1, 8)
fromVulgar '\x215c' = Just (3, 8)
fromVulgar '\x215d' = Just (5, 8)
fromVulgar '\x215e' = Just (7, 8)
fromVulgar '\x2189' = Just (0, 3)  -- used in baseball
fromVulgar _ = Nothing

-- | Try to convert the given 'Char', if it is a /vulgar fraction/, to a 'Ratio' object. Returns 'Nothing' if the 'Char' is not a vulgar fraction character.
fromVulgarToRatio :: Integral i
  => Char  -- ^ The character to decode.
  -> Maybe (Ratio i)  -- ^ The corresponding 'Ratio' wrapped in a 'Just' if the ratio is a vulgar fraction, 'Nothing' otherwise.
fromVulgarToRatio = fmap (uncurry (%)) . fromVulgar

-- | Try to parse the text as a /vulgar fraction/ and fallback on the 'unicodetoRatioParts' function to parse it as a fraction.
fromVulgarFallback :: (Read i, Integral i, Read j, Integral j)
  => Text  -- ^ The 'Text' we try to decode as a (vulgar) fraction.
  -> Maybe (i, j)  -- ^ A 2-tuple with the numerator and denominator wrapped in a 'Just' if the 'Text' can be parsed, 'Nothing' otherwise.
fromVulgarFallback d = _attm0 d' <|> _attm1 d' <|> unicodeToRatioParts d
  where d' = unpack d
        _attm0 [d0] = fromVulgar d0
        _attm0 _ = Nothing
        _attm1 ('\x215f':ds) = (1,) <$> readMaybe (map fromSubSup ds)
        _attm1 _ = Nothing

-- | Try to parse the text as a /vulgar fraction/ and fallback on the 'unicodetoRatioParts' function to parse it as a fraction.
fromVulgarFallbackToRatio :: (Read i, Integral i)
  => Text  -- ^ The 'Text' we try to decode as a (vulgar) fraction.
  -> Maybe (Ratio i)  -- ^ The parsed 'Ratio' wrapped in a 'Just' if the 'Text' can be parsed, 'Nothing' otherwise.
fromVulgarFallbackToRatio = fmap (uncurry (%)) . fromVulgarFallback

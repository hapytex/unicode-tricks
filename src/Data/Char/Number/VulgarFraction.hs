{-# LANGUAGE Safe #-}

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
    -- * Render to a vulgar fraction, with a fallback to using small characters
  , ratioToVulgarFallback, toVulgarFallback
  ) where

import Data.Ratio(Ratio, numerator, denominator)
import Data.Text(Text, cons, singleton)

import Data.Char.Small(asSub', ratioPartsToUnicode')

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

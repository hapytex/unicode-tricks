{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Math.Internal
Description : A module to write math unicode alphanumerical characters.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Internal functions for the Math module.
-}

module Data.Char.Math.Internal
  ( _shiftC
  , _ordc
  , _baseUpperLower
  , _baseUpperLowerNum
  , _isValidInt
  , _withCondition
  , _dispatchLatinGreekDigit
  , _dispatchLatinGreekDigit'
  ) where

import Data.Char (chr, isAsciiUpper, isDigit, ord)
import Data.Char.Core(isAsciiAlpha, isGreek)

_shiftC :: Int -> Char -> Char
_shiftC = (chr .) . (. ord) . (+)

_ordc :: Char -> Int -> Char
_ordc = (chr .) . (+) . ord

_baseUpperLower :: Int -> Char -> Char
_baseUpperLower b c
    | isAsciiUpper c = oc (b+6)
    | otherwise = oc b
    where oc = _ordc c

_baseUpperLowerNum :: Int -> Int -> Char -> Char
_baseUpperLowerNum n b c
    | isDigit c = oc n
    | isAsciiUpper c = oc (b+6)
    | otherwise = oc b
    where oc = _ordc c

_isValidInt :: Int -> Bool
_isValidInt x = x < 10 && x >= 0

_withCondition :: (a -> Bool) -> (a -> b) -> a -> Maybe b
_withCondition p f = go
    where go x | p x = Just (f x)
               | otherwise = Nothing

-- | Unsafe dispatch to a conversion function according to the character
_dispatchLatinGreekDigit'
    :: (Char -> b) -- ^ Latin
    -> (Char -> b) -- ^ Greek
    -> (Char -> b) -- ^ Digit
    -> Char        -- ^ Character to transform
    -> b
_dispatchLatinGreekDigit' l g d c
  | isAsciiAlpha c = l c
  | isGreek c      = g c
  | otherwise      = d c

-- | Safe dispatch to a conversion function according to the character
_dispatchLatinGreekDigit
    :: (Char -> b) -- ^ Latin
    -> (Char -> b) -- ^ Greek
    -> (Char -> b) -- ^ Digit
    -> Char        -- ^ Character to transform
    -> Maybe b
_dispatchLatinGreekDigit l g d c
  | isAsciiAlpha c = Just (l c)
  | isGreek c      = Just (g c)
  | isDigit c      = Just (d c)
  | otherwise      = Nothing
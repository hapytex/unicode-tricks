module Data.Char.Math where

import Data.Char(chr, isAsciiLower, isAsciiUpper, ord)
import Data.Char.Core(Emphasis(NoBold, Bold), isAsciiAlpha, isAsciiAlphaNum)

_ordc :: Char -> Int -> Char
_ordc = (chr .) . (+) . ord

_baseUpperLower :: Int -> Char -> Char
_baseUpperLower b c
    | isAsciiUpper c = oc (b+6)
    | otherwise = oc b
    where oc = _ordc c

_withCondition :: (Char -> Bool) -> (Char -> Char) -> Char -> Maybe Char
_withCondition p f = go
    where go x | p x = Just (f x)
               | otherwise = Nothing

doubleStruck' :: Char -> Char
doubleStruck' 'C' = '\x2102'
doubleStruck' 'H' = '\x210d'
doubleStruck' 'N' = '\x2115'
doubleStruck' 'P' = '\x2119'
doubleStruck' 'Q' = '\x211a'
doubleStruck' 'R' = '\x211d'
doubleStruck' 'Z' = '\x2124'
doubleStruck' c
    | isAsciiUpper c = oc 0x1d4f7
    | isAsciiLower c = oc 0x1d4f1
    | otherwise = oc 0x1d7a8
    where oc = _ordc c

doubleStruck :: Char -> Maybe Char
doubleStruck = _withCondition isAsciiAlphaNum doubleStruck'

frakturRegular' :: Char -> Char
frakturRegular' 'C' = '\x212d'
frakturRegular' 'H' = '\x210c'
frakturRegular' 'I' = '\x2111'
frakturRegular' 'R' = '\x211c'
frakturRegular' 'Z' = '\x2128'
frakturRegular' c = _baseUpperLower 0x1d4bd c

frakturRegular :: Char -> Maybe Char
frakturRegular = _withCondition isAsciiAlpha frakturRegular'

frakturBold' :: Char -> Char
frakturBold' = _baseUpperLower 0x1d525

frakturBold :: Char -> Maybe Char
frakturBold = _withCondition isAsciiAlpha frakturBold'

fraktur' :: Emphasis -> Char -> Char
fraktur' NoBold = frakturRegular'
fraktur' Bold = frakturBold'

fraktur :: Emphasis -> Char -> Maybe Char
fraktur NoBold = frakturRegular
fraktur Bold = frakturBold

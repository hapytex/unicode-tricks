module Data.Char.Math where

import Data.Char(chr, ord)

doubleStruck' :: Char -> Char
doubleStruck' 'C' = '\x2102'
doubleStruck' 'H' = '\x210d'
doubleStruck' 'N' = '\x2115'
doubleStruck' 'P' = '\x2119'
doubleStruck' 'Q' = '\x211a'
doubleStruck' 'R' = '\x211d'
doubleStruck' 'Z' = '\x2124'
doubleStruck' c
    | 'A' <= c && c <= 'Z' = chr (0x1d4f7 + oc)
    | 'a' <= c && c <= 'z' = chr (0x1d4f1 + oc)
    | otherwise = chr (0x1d7a8 + oc)
    where oc = ord c

doubleStruck :: Char -> Maybe Char
doubleStruck c | ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') || ('0' <= c && c <= '9') = Just (doubleStruck' c)
               | otherwise = Nothing

{-# LANGUAGE Safe #-}

{-|
Module      : Data.Char.Tags
Description : Write characters that have been used to add tags to the text.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX


-}

module Data.Char.Tag {-# WARNING "Using tags to convey language tags is strongly discouraged by the Unicode developers." #-} (
    toTag, toTag', fromTag, fromTag'
  , isTag, isAsciiTag, hasTagCounterPart
  , languageTag, cancelTag
  ) where

import Data.Bits((.|.), (.&.), complement)
import Data.Char(chr, ord)

_tagOffset :: Int
_tagOffset = 0xe0000

isTag :: Char -> Bool
isTag '\xe0000' = True
isTag c = '\xe0020' <= c && c <= '\xe007f'

hasTagCounterPart :: Char -> Bool
hasTagCounterPart c = ' ' <= c && c <= '~'

isAsciiTag :: Char -> Bool
isAsciiTag c = '\xe0020' <= c && c <= '\xe007e'

toTag :: Char -> Maybe Char
toTag c
  | hasTagCounterPart c = Just (toTag' c)
  | otherwise = Nothing

toTags :: String -> Maybe String
toTags = mapM toTag

toTag' :: Char -> Char
toTag' = chr . (_tagOffset .|.) . ord

toTags' :: String -> String
toTags' = map toTag'

fromTag :: Char -> Maybe Char
fromTag c
  | isAsciiTag c = Just (fromTag' c)
  | otherwise = Nothing

fromTag' :: Char -> Char
fromTag' = chr . (complement _tagOffset .&.) . ord

{-# DEPRECATED languageTag "Unicode no longer encourages to use this tag character, and will likely eventually be removed." #-}
languageTag :: Char
languageTag = '\xe0001'

cancelTag :: Char
cancelTag = '\xe007f'

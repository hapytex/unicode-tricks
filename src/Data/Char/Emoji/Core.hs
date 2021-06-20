{-# LANGUAGE PatternSynonyms, Safe #-}

{-|
Module      : Data.Char.Emoji.Core
Description : A module that defines basic properties applicable to all /Emoji/.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines Emoji patterns.
-}

module Data.Char.Emoji.Core (
    -- * Pattern synonyms
    pattern EmojiSuffix
  ) where

-- | A 'Char'acter that is often used as a suffix to turn a character into an
-- emoji.
pattern EmojiSuffix :: Char
pattern EmojiSuffix = '\xfe0f'

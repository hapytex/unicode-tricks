{-# LANGUAGE PatternSynonyms, Safe #-}

module Data.Char.Emoji.Core where

-- | A 'Char'acter that is often used as a suffix to turn a character into an
-- emoji.
pattern EmojiSuffix :: Char
pattern EmojiSuffix = '\xfe0f'

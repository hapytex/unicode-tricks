{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternSynonyms, Safe, TypeApplication #-}

{-|
Module      : Data.Char.Emoji.Zodiac
Description : A module that defines zodiac emoji together with the English names as pattern synonyms.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has emojis for the twelve zodiac signs. In this module a data type is used to present the zodiac
emoji and also defines pattern synonyms for these.
-}


module Data.Char.Emoji.Zodiac (
    -- * Zodiac datatype
    Zodiac(Aries, Taurus, Gemini, Cancer, Leo, Virgo, Libra, Scorpio, Sagittarius, Capricorn, Aquarius, Pisces)
    -- * Pattern aliasses
  , pattern Ram, pattern Bull, pattern Twins, pattern Crab, pattern Lion, pattern Maiden, pattern Scales, pattern Scorpius, pattern Scorpion
  , pattern Centaur, pattern Archer, pattern Capricornus, pattern MountainGoat, pattern GoatHorned, pattern SeaGoat, pattern WaterBearer
  , pattern Fish
  ) where

import Control.DeepSeq(NFData)

import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar', isInCharRange), UnicodeText(isInTextRange), generateIsInTextRange', mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_zodiacOffset :: Int
_zodiacOffset = 0x2648

-- | A data type to deal with the /zodiac sign/ emoji. The data type lists the
-- different zodiac signs as data constructors, and the instance of the
-- 'UnicodeCharacter' allows to convert it from and to a 'Char'acter.
data Zodiac
  = Aries  -- ^ The /aries/ zodiac sign, /ram/ in English, is denoted as ♈.
  | Taurus  -- ^ The /taurus/ zodiac sign, /bull/ in English, is denoted as ♉.
  | Gemini  -- ^ The /gemini/ zodiac sign, /twins/ in English, is denoted as ♊.
  | Cancer  -- ^ The /cancer/ zodiac sign, /crab/ in English, is denoted as ♋.
  | Leo  -- ^ The /leo/ zodiac sign, /lion/ in English, is denoted as ♌.
  | Virgo  -- ^ The /virgo/ zodiac sign, /maiden/ in English, is denoted as ♍.
  | Libra  -- ^ The /libra/ zodiac sign, /scales/ in English, is denoted as ♎.
  | Scorpio  -- ^ The /scorpio/ zodiac sign, /scorpion/ in English, is denoted as ♏.
  | Sagittarius  -- ^ The /saggitarius/ zodiac sign, /archer/ in English, is denoted as ♐.
  | Capricorn  -- ^ The /capricorn/ zodiac sign, /sea-goat/ in English, is denoted as ♑.
  | Aquarius  -- ^ The /aquarius/ zodiac sign, /water-bearer/ in English, is denoted as ♒.
  | Pisces  -- ^ The /pices/ zodiac sign, /fish/ in English, is denoted as ♓.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary Zodiac where
    arbitrary = arbitraryBoundedEnum

instance Hashable Zodiac

instance NFData Zodiac

instance UnicodeCharacter Zodiac where
    toUnicodeChar = mapFromEnum _zodiacOffset
    fromUnicodeChar = mapToEnumSafe _zodiacOffset
    fromUnicodeChar' = mapToEnum _zodiacOffset
    isInCharRange c = '\x2648' <= c && c <= '\x2654'

instance UnicodeText Zodiac where
    isInTextRange = generateIsInTextRange' @Zodiac

-- | The English name for the 'Aries' zodiac sign.
pattern Ram :: Zodiac
pattern Ram = Aries

-- | The English name for the 'Taurus' zodiac sign.
pattern Bull :: Zodiac
pattern Bull = Taurus

-- | The English name for the 'Gemini' zodiac sign.
pattern Twins :: Zodiac
pattern Twins = Gemini

-- | The English name for the 'Cancer' zodiac sign.
pattern Crab :: Zodiac
pattern Crab = Cancer

-- | The English name for the 'Leo' zodiac sign.
pattern Lion :: Zodiac
pattern Lion = Leo

-- | The English name for the 'Virgo' zodiac sign.
pattern Maiden :: Zodiac
pattern Maiden = Virgo

-- | The English name for the 'Libra' zodiac sign.
pattern Scales :: Zodiac
pattern Scales = Libra

-- | The name of the constellation of the 'Scorpio' zodiac sign.
pattern Scorpius :: Zodiac
pattern Scorpius = Scorpio

-- | The English name for the 'Scorpio' zodiac sign.
pattern Scorpion :: Zodiac
pattern Scorpion = Scorpio

-- | An English name for the 'Sagittarius' zodiac sign.
pattern Centaur :: Zodiac
pattern Centaur = Sagittarius

-- | An English name for the 'Sagittarius' zodiac sign.
pattern Archer :: Zodiac
pattern Archer = Sagittarius

-- | The name of the constellation of the 'Capricorn' zodiac sign.
pattern Capricornus :: Zodiac
pattern Capricornus = Capricorn

-- | An English name for the 'Capricorn' zodiac sign.
pattern MountainGoat :: Zodiac
pattern MountainGoat = Capricorn

-- | An English name for the 'Capricorn' zodiac sign.
pattern GoatHorned :: Zodiac
pattern GoatHorned = Capricorn

-- | An English name for the 'Capricorn' zodiac sign.
pattern SeaGoat :: Zodiac
pattern SeaGoat = Capricorn

-- | The English name for the 'Aquarius' zodiac sign.
pattern WaterBearer :: Zodiac
pattern WaterBearer = Aquarius

-- | The English name for the 'Pisces' zodiac sign.
pattern Fish :: Zodiac
pattern Fish = Pisces


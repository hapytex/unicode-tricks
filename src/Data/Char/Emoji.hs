{-# LANGUAGE PatternSynonyms, Safe #-}

{-|
Module      : Data.Char.Emoji
Description : A module that defines emoji and ways to render and modify emoji.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode defines 2182 emoji characters, this module aims to make working with emoji characters more convenient.
-}

module Data.Char.Emoji (
    -- * Flag emoji
    iso3166Alpha2ToFlag', validFlagEmoji
    -- * Zodiac emoji
  , Zodiac(Aries, Taurus, Gemini, Cancer, Leo, Virgo, Libra, Scorpio, Sagittarius, Capricorn, Aquarius, Pisces)
    -- * Skin color modifier
  , SkinColorModifier(Light, MediumLight, Medium, MediumDark, Dark), fromFitzPatrick
    -- * Pattern synonyms for emoji elements
  , pattern Ram, pattern Bull, pattern Twins, pattern Crab, pattern Lion, pattern Maiden, pattern Scales, pattern Scorpius, pattern Scorpion
  , pattern Centaur, pattern Archer, pattern Capricornus, pattern MountainGoat, pattern GoatHorned, pattern SeaGoat, pattern WaterBearer
  , pattern Fish
  , pattern FitzPatrickI, pattern FitzPatrickII, pattern FitzPatrickIII, pattern FitzPatrickIV, pattern FitzPatrickV, pattern FitzPatrickVI
  ) where

import Data.Char(toUpper)
import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar'), mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Text(Text, pack)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

_skinColorOffset :: Int
_skinColorOffset = 0x1f3fb

_zodiacOffset :: Int
_zodiacOffset = 0x2648

-- | Some emoji deal with people. One can change the color of the skin with the
-- 'SkinColorModifier'. For the skin color, the <https://en.wikipedia.org/wiki/Fitzpatrick_scale /Fitzpatrick scale/> is used.
-- A numerical classification system for skin types.
data SkinColorModifier
  = Light  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ one or two to the Emoji.
  | MediumLight  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ three to the Emoji.
  | Medium  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ four to the Emoji.
  | MediumDark  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ five to the Emoji.
  | Dark  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ six to the Emoji.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

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
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | The 'SkinColorModifier' that corresponds to type one of the /Fitzpatrick
-- scale/.
pattern FitzPatrickI :: SkinColorModifier
pattern FitzPatrickI = Light

-- | The 'SkinColorModifier' that corresponds to type two of the /Fitzpatrick
-- scale/.
pattern FitzPatrickII :: SkinColorModifier
pattern FitzPatrickII = Light

-- | The 'SkinColorModifier' that corresponds to type three of the /Fitzpatrick
-- scale/.
pattern FitzPatrickIII :: SkinColorModifier
pattern FitzPatrickIII = MediumLight

-- | The 'SkinColorModifier' that corresponds to type four of the /Fitzpatrick
-- scale/.
pattern FitzPatrickIV :: SkinColorModifier
pattern FitzPatrickIV = Medium

-- | The 'SkinColorModifier' that corresponds to type five of the /Fitzpatrick
-- scale/.
pattern FitzPatrickV :: SkinColorModifier
pattern FitzPatrickV = MediumDark

-- | The 'SkinColorModifier' that corresponds to type six of the /Fitzpatrick
-- scale/.
pattern FitzPatrickVI :: SkinColorModifier
pattern FitzPatrickVI = Dark

-- | Convert the given /Fitzpatrick skin type/ to the corresponding
-- 'SkinColorModifier' wrapped in a 'Just', if no such 'SkinColorModifier'
-- exists, 'Nothing' is returned.
fromFitzPatrick :: Integral i
  => i  -- ^ The given /Fitzpatrick skin type/.
  -> Maybe SkinColorModifier  -- ^ The corresponding 'SkinColorModifier' wrapped in a 'Just'; 'Nothing' if no such modifier exists.
fromFitzPatrick 1 = Just Light
fromFitzPatrick 2 = Just Light
fromFitzPatrick 3 = Just MediumLight
fromFitzPatrick 4 = Just Medium
fromFitzPatrick 5 = Just MediumDark
fromFitzPatrick 6 = Just Dark
fromFitzPatrick _ = Nothing

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

-- | Convert the given two 'Char'acters of the ISO3166-1 Alpha-2 standard to an
-- Emoji that renders the flag of the corresponding country or terroitory.
-- Deprecated regions, such as SU (Soviet Union) and YU (Yugoslavia). The
-- European Union (EU), Antarctica (AQ) and United Nations (UN) are included
-- as marcoregion flags. This function does not check if the two characters
-- map to a valid flag token.
iso3166Alpha2ToFlag'
  :: Char  -- ^ The first 'Char'acter of the ISO3166 Alpha-2 code.
  -> Char  -- ^ The second 'Char'acter of the ISO3166 Alpha-2 code.
  -> Text  -- ^ A 'Text' object that consists of two characters, where the two characters form a flag emoji, if the given flag exists.
iso3166Alpha2ToFlag' ca cb = pack (map (mapFromEnum 0x1f1a5 . toUpper) [ca, cb])

-- | Check if for the given two 'Char'acters, a flag emoji exists. The two
-- character combinations for which a flag exist are defined in the ISO3166-1
-- Alpha-2 standard where deprecated reagions, such as SU and YU have no flag,
-- and the European Union (EU), Antarctica (AQ), and the United Nations (UN)
-- have a flag.
validFlagEmoji
  :: Char  -- ^ The first 'Char'acter of the ISO3166 Alpha-2 code.
  -> Char  -- ^ The second 'Char'acter of the ISO3166 Alpha-2 code.
  -> Bool  -- ^ 'True' if a flag emoji exists for the given characters; 'False' otherwise.
validFlagEmoji 'A' 'I' = True
validFlagEmoji 'A' 'O' = True
validFlagEmoji 'A' 'Z' = True
validFlagEmoji 'A' 'L' = True
validFlagEmoji 'A' 'M' = True
validFlagEmoji 'A' 'W' = True
validFlagEmoji 'A' 'X' = True
validFlagEmoji 'A' x = ('C' <= x && x <= 'G') || ('Q' <= x && x <= 'U')
validFlagEmoji 'B' 'A' = True
validFlagEmoji 'B' 'B' = True
validFlagEmoji 'B' 'V' = True
validFlagEmoji 'B' 'W' = True
validFlagEmoji 'B' 'Y' = True
validFlagEmoji 'B' 'Z' = True
validFlagEmoji 'B' x = ('D' <= x && x <= 'J') || ('L' <= x && x <= 'O') || ('Q' <= x && x <= 'T')
validFlagEmoji 'C' 'A' = True
validFlagEmoji 'C' 'R' = True
validFlagEmoji 'C' 'C' = True
validFlagEmoji 'C' 'D' = True
validFlagEmoji 'C' x = ('K' <= x && x <= 'P') || ('U' <= x && x <= 'Z') || ('F' <= x && x <= 'I')
validFlagEmoji 'D' 'E' = True
validFlagEmoji 'D' 'G' = True
validFlagEmoji 'D' 'M' = True
validFlagEmoji 'D' 'O' = True
validFlagEmoji 'D' 'Z' = True
validFlagEmoji 'D' 'J' = True
validFlagEmoji 'D' 'K' = True
validFlagEmoji 'E' 'A' = True
validFlagEmoji 'E' 'C' = True
validFlagEmoji 'E' 'E' = True
validFlagEmoji 'E' 'G' = True
validFlagEmoji 'E' 'H' = True
validFlagEmoji 'E' x = 'R' <= x && x <= 'U'
validFlagEmoji 'F' 'M' = True
validFlagEmoji 'F' 'O' = True
validFlagEmoji 'F' 'R' = True
validFlagEmoji 'F' x = 'I' <= x && x <= 'K'
validFlagEmoji 'G' 'W' = True
validFlagEmoji 'G' 'Y' = True
validFlagEmoji 'G' 'A' = True
validFlagEmoji 'G' 'B' = True
validFlagEmoji 'G' x = ('D' <= x && x <= 'I') || ('P' <= x && x <= 'U') || ('L' <= x && x <= 'N')
validFlagEmoji 'H' 'K' = True
validFlagEmoji 'H' 'R' = True
validFlagEmoji 'H' 'M' = True
validFlagEmoji 'H' 'N' = True
validFlagEmoji 'H' 'T' = True
validFlagEmoji 'H' 'U' = True
validFlagEmoji 'I' x = ('L' <= x && x <= 'O') || ('Q' <= x && x <= 'T') || ('C' <= x && x <= 'E')
validFlagEmoji 'J' 'E' = True
validFlagEmoji 'J' 'M' = True
validFlagEmoji 'J' 'O' = True
validFlagEmoji 'J' 'P' = True
validFlagEmoji 'K' 'E' = True
validFlagEmoji 'K' 'P' = True
validFlagEmoji 'K' 'R' = True
validFlagEmoji 'K' 'W' = True
validFlagEmoji 'K' 'M' = True
validFlagEmoji 'K' 'N' = True
validFlagEmoji 'K' 'Y' = True
validFlagEmoji 'K' 'Z' = True
validFlagEmoji 'K' x = 'G' <= x && x <= 'I'
validFlagEmoji 'L' 'I' = True
validFlagEmoji 'L' 'K' = True
validFlagEmoji 'L' 'Y' = True
validFlagEmoji 'L' x = ('R' <= x && x <= 'V') || ('A' <= x && x <= 'C')
validFlagEmoji 'M' 'A' = True
validFlagEmoji 'M' x = ('K' <= x && x <= 'Z') || ('C' <= x && x <= 'H')
validFlagEmoji 'N' 'A' = True
validFlagEmoji 'N' 'C' = True
validFlagEmoji 'N' 'I' = True
validFlagEmoji 'N' 'L' = True
validFlagEmoji 'N' 'R' = True
validFlagEmoji 'N' 'U' = True
validFlagEmoji 'N' 'Z' = True
validFlagEmoji 'N' 'O' = True
validFlagEmoji 'N' 'P' = True
validFlagEmoji 'N' x = 'E' <= x && x <= 'G'
validFlagEmoji 'O' 'M' = True
validFlagEmoji 'P' 'A' = True
validFlagEmoji 'P' 'W' = True
validFlagEmoji 'P' 'Y' = True
validFlagEmoji 'P' x = ('E' <= x && x <= 'H') || ('K' <= x && x <= 'N') || ('R' <= x && x <= 'T')
validFlagEmoji 'Q' 'A' = True
validFlagEmoji 'R' 'E' = True
validFlagEmoji 'R' 'O' = True
validFlagEmoji 'R' 'S' = True
validFlagEmoji 'R' 'U' = True
validFlagEmoji 'R' 'W' = True
validFlagEmoji 'S' 'V' = True
validFlagEmoji 'S' x = ('G' <= x && x <= 'O') || ('A' <= x && x <= 'E') || ('R' <= x && x <= 'T') || ('X' <= x && x <= 'Z')
validFlagEmoji 'T' 'A' = True
validFlagEmoji 'T' 'R' = True
validFlagEmoji 'T' 'T' = True
validFlagEmoji 'T' 'Z' = True
validFlagEmoji 'T' 'C' = True
validFlagEmoji 'T' 'D' = True
validFlagEmoji 'T' 'V' = True
validFlagEmoji 'T' 'W' = True
validFlagEmoji 'T' x = ('J' <= x && x <= 'O') || ('F' <= x && x <= 'H')
validFlagEmoji 'U' 'A' = True
validFlagEmoji 'U' 'G' = True
validFlagEmoji 'U' 'S' = True
validFlagEmoji 'U' 'M' = True
validFlagEmoji 'U' 'N' = True
validFlagEmoji 'U' 'Y' = True
validFlagEmoji 'U' 'Z' = True
validFlagEmoji 'V' 'A' = True
validFlagEmoji 'V' 'C' = True
validFlagEmoji 'V' 'E' = True
validFlagEmoji 'V' 'G' = True
validFlagEmoji 'V' 'I' = True
validFlagEmoji 'V' 'N' = True
validFlagEmoji 'V' 'U' = True
validFlagEmoji 'W' 'F' = True
validFlagEmoji 'W' 'S' = True
validFlagEmoji 'X' 'K' = True
validFlagEmoji 'Y' 'E' = True
validFlagEmoji 'Y' 'T' = True
validFlagEmoji 'Z' 'A' = True
validFlagEmoji 'Z' 'M' = True
validFlagEmoji 'Z' 'W' = True
validFlagEmoji _ _ = False

instance Arbitrary SkinColorModifier where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Zodiac where
    arbitrary = arbitraryBoundedEnum

instance UnicodeCharacter SkinColorModifier where
    toUnicodeChar = mapFromEnum _skinColorOffset
    fromUnicodeChar = mapToEnumSafe _skinColorOffset
    fromUnicodeChar' = mapToEnum _skinColorOffset

instance UnicodeCharacter Zodiac where
    toUnicodeChar = mapFromEnum _zodiacOffset
    fromUnicodeChar = mapToEnumSafe _zodiacOffset
    fromUnicodeChar' = mapToEnum _zodiacOffset

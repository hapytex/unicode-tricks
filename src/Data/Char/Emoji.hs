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
    iso3166Alpha2ToFlag, iso3166Alpha2ToFlag', validFlagEmoji
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
import Data.Function(on)
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
-- Deprecated regions, such as SU (Soviet Union) and YU (Yugoslavia) have no
-- flag. The European Union (EU), Antarctica (AQ) and United Nations (UN)
-- are included as marcoregion flags. This function does not check if the
-- two characters map to a valid flag token.
iso3166Alpha2ToFlag'
  :: Char  -- ^ The first 'Char'acter of the ISO3166 Alpha-2 code.
  -> Char  -- ^ The second 'Char'acter of the ISO3166 Alpha-2 code.
  -> Text  -- ^ A 'Text' object that consists of two characters, where the two characters form a flag emoji, if the given flag exists.
iso3166Alpha2ToFlag' ca cb = pack (map (mapFromEnum 0x1f1a5 . toUpper) [ca, cb])

-- | Convert the given two 'Char'acters of the ISO3166-1 Alpha-2 standard to an
-- Emoji that renders the flag of the corresponding country or terroitory
-- wrapped in a 'Just' data constructor. Deprecated regions, such as SU
-- (Soviet Union) and YU (Yugoslavia) have no flag. The European Union (EU),
-- Antarctica (AQ) and United Nations (UN) are included as marcoregion flags.
-- If the flag does not exists, 'Nothing' is returned.
iso3166Alpha2ToFlag
  :: Char  -- ^ The first 'Char'acter of the ISO3166 Alpha-2 code.
  -> Char  -- ^ The second 'Char'acter of the ISO3166 Alpha-2 code.
  -> Maybe Text  -- ^ A 'Text' object that consists of two characters, where the two characters form a flag emoji wrapped in a 'Just', if the given flag exists; 'Nothing' otherwise.
iso3166Alpha2ToFlag ca cb
  | validFlagEmoji ca cb = Just (iso3166Alpha2ToFlag' ca cb)
  | otherwise = Nothing

-- | Check if for the given two 'Char'acters, a flag emoji exists. The two
-- character combinations for which a flag exist are defined in the ISO3166-1
-- Alpha-2 standard where deprecated reagions, such as SU and YU have no flag,
-- and the European Union (EU), Antarctica (AQ), and the United Nations (UN)
-- have a flag.
validFlagEmoji
  :: Char  -- ^ The first 'Char'acter of the ISO3166 Alpha-2 code.
  -> Char  -- ^ The second 'Char'acter of the ISO3166 Alpha-2 code.
  -> Bool  -- ^ 'True' if a flag emoji exists for the given characters; 'False' otherwise.
validFlagEmoji = on _validFlagEmoji toUpper

_validFlagEmoji 'A' 'I' = True
_validFlagEmoji 'A' 'O' = True
_validFlagEmoji 'A' 'Z' = True
_validFlagEmoji 'A' 'L' = True
_validFlagEmoji 'A' 'M' = True
_validFlagEmoji 'A' 'W' = True
_validFlagEmoji 'A' 'X' = True
_validFlagEmoji 'A' x = ('C' <= x && x <= 'G') || ('Q' <= x && x <= 'U')
_validFlagEmoji 'B' 'A' = True
_validFlagEmoji 'B' 'B' = True
_validFlagEmoji 'B' 'V' = True
_validFlagEmoji 'B' 'W' = True
_validFlagEmoji 'B' 'Y' = True
_validFlagEmoji 'B' 'Z' = True
_validFlagEmoji 'B' x = ('D' <= x && x <= 'J') || ('L' <= x && x <= 'O') || ('Q' <= x && x <= 'T')
_validFlagEmoji 'C' 'A' = True
_validFlagEmoji 'C' 'R' = True
_validFlagEmoji 'C' 'C' = True
_validFlagEmoji 'C' 'D' = True
_validFlagEmoji 'C' x = ('K' <= x && x <= 'P') || ('U' <= x && x <= 'Z') || ('F' <= x && x <= 'I')
_validFlagEmoji 'D' 'E' = True
_validFlagEmoji 'D' 'G' = True
_validFlagEmoji 'D' 'M' = True
_validFlagEmoji 'D' 'O' = True
_validFlagEmoji 'D' 'Z' = True
_validFlagEmoji 'D' 'J' = True
_validFlagEmoji 'D' 'K' = True
_validFlagEmoji 'E' 'A' = True
_validFlagEmoji 'E' 'C' = True
_validFlagEmoji 'E' 'E' = True
_validFlagEmoji 'E' 'G' = True
_validFlagEmoji 'E' 'H' = True
_validFlagEmoji 'E' x = 'R' <= x && x <= 'U'
_validFlagEmoji 'F' 'M' = True
_validFlagEmoji 'F' 'O' = True
_validFlagEmoji 'F' 'R' = True
_validFlagEmoji 'F' x = 'I' <= x && x <= 'K'
_validFlagEmoji 'G' 'W' = True
_validFlagEmoji 'G' 'Y' = True
_validFlagEmoji 'G' 'A' = True
_validFlagEmoji 'G' 'B' = True
_validFlagEmoji 'G' x = ('D' <= x && x <= 'I') || ('P' <= x && x <= 'U') || ('L' <= x && x <= 'N')
_validFlagEmoji 'H' 'K' = True
_validFlagEmoji 'H' 'R' = True
_validFlagEmoji 'H' 'M' = True
_validFlagEmoji 'H' 'N' = True
_validFlagEmoji 'H' 'T' = True
_validFlagEmoji 'H' 'U' = True
_validFlagEmoji 'I' x = ('L' <= x && x <= 'O') || ('Q' <= x && x <= 'T') || ('C' <= x && x <= 'E')
_validFlagEmoji 'J' 'E' = True
_validFlagEmoji 'J' 'M' = True
_validFlagEmoji 'J' 'O' = True
_validFlagEmoji 'J' 'P' = True
_validFlagEmoji 'K' 'E' = True
_validFlagEmoji 'K' 'P' = True
_validFlagEmoji 'K' 'R' = True
_validFlagEmoji 'K' 'W' = True
_validFlagEmoji 'K' 'M' = True
_validFlagEmoji 'K' 'N' = True
_validFlagEmoji 'K' 'Y' = True
_validFlagEmoji 'K' 'Z' = True
_validFlagEmoji 'K' x = 'G' <= x && x <= 'I'
_validFlagEmoji 'L' 'I' = True
_validFlagEmoji 'L' 'K' = True
_validFlagEmoji 'L' 'Y' = True
_validFlagEmoji 'L' x = ('R' <= x && x <= 'V') || ('A' <= x && x <= 'C')
_validFlagEmoji 'M' 'A' = True
_validFlagEmoji 'M' x = ('K' <= x && x <= 'Z') || ('C' <= x && x <= 'H')
_validFlagEmoji 'N' 'A' = True
_validFlagEmoji 'N' 'C' = True
_validFlagEmoji 'N' 'I' = True
_validFlagEmoji 'N' 'L' = True
_validFlagEmoji 'N' 'R' = True
_validFlagEmoji 'N' 'U' = True
_validFlagEmoji 'N' 'Z' = True
_validFlagEmoji 'N' 'O' = True
_validFlagEmoji 'N' 'P' = True
_validFlagEmoji 'N' x = 'E' <= x && x <= 'G'
_validFlagEmoji 'O' 'M' = True
_validFlagEmoji 'P' 'A' = True
_validFlagEmoji 'P' 'W' = True
_validFlagEmoji 'P' 'Y' = True
_validFlagEmoji 'P' x = ('E' <= x && x <= 'H') || ('K' <= x && x <= 'N') || ('R' <= x && x <= 'T')
_validFlagEmoji 'Q' 'A' = True
_validFlagEmoji 'R' 'E' = True
_validFlagEmoji 'R' 'O' = True
_validFlagEmoji 'R' 'S' = True
_validFlagEmoji 'R' 'U' = True
_validFlagEmoji 'R' 'W' = True
_validFlagEmoji 'S' 'V' = True
_validFlagEmoji 'S' x = ('G' <= x && x <= 'O') || ('A' <= x && x <= 'E') || ('R' <= x && x <= 'T') || ('X' <= x && x <= 'Z')
_validFlagEmoji 'T' 'A' = True
_validFlagEmoji 'T' 'R' = True
_validFlagEmoji 'T' 'T' = True
_validFlagEmoji 'T' 'Z' = True
_validFlagEmoji 'T' 'C' = True
_validFlagEmoji 'T' 'D' = True
_validFlagEmoji 'T' 'V' = True
_validFlagEmoji 'T' 'W' = True
_validFlagEmoji 'T' x = ('J' <= x && x <= 'O') || ('F' <= x && x <= 'H')
_validFlagEmoji 'U' 'A' = True
_validFlagEmoji 'U' 'G' = True
_validFlagEmoji 'U' 'S' = True
_validFlagEmoji 'U' 'M' = True
_validFlagEmoji 'U' 'N' = True
_validFlagEmoji 'U' 'Y' = True
_validFlagEmoji 'U' 'Z' = True
_validFlagEmoji 'V' 'A' = True
_validFlagEmoji 'V' 'C' = True
_validFlagEmoji 'V' 'E' = True
_validFlagEmoji 'V' 'G' = True
_validFlagEmoji 'V' 'I' = True
_validFlagEmoji 'V' 'N' = True
_validFlagEmoji 'V' 'U' = True
_validFlagEmoji 'W' 'F' = True
_validFlagEmoji 'W' 'S' = True
_validFlagEmoji 'X' 'K' = True
_validFlagEmoji 'Y' 'E' = True
_validFlagEmoji 'Y' 'T' = True
_validFlagEmoji 'Z' 'A' = True
_validFlagEmoji 'Z' 'M' = True
_validFlagEmoji 'Z' 'W' = True
_validFlagEmoji _ _ = False

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

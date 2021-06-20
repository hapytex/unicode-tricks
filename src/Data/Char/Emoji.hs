{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, PatternSynonyms, OverloadedStrings, Safe #-}

{-|
Module      : Data.Char.Emoji
Description : A module that defines emoji and ways to render and modify emoji.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode defines 2182 emoji characters, this module aims to make working with emoji characters more convenient.
-}

module Data.Char.Emoji (
    -- * Emoji suffix
    pattern EmojiSuffix
    -- * Clock emoji
  , Clock, hours, minutes30, clock, closestClock
    -- * Blood type emoji
  , BloodType(O, B, A, AB)
    -- * Moon phase emoji
  , MoonPhase(NewMoon, WaxingCrescent, FirstQuarter, WaxingGibbous, FullMoon, WaningGibbous, ThirdQuarter, WaningCrescent)
    -- * Gender sign emoji
  , Gender(Female, Male)
    -- * Zodiac emoji
  , Zodiac(Aries, Taurus, Gemini, Cancer, Leo, Virgo, Libra, Scorpio, Sagittarius, Capricorn, Aquarius, Pisces)
    -- * Skin color modifier
  , SkinColorModifier(Light, MediumLight, Medium, MediumDark, Dark), fromFitzPatrick
    -- * Pattern synonyms for 'Zodiac' elements
  , pattern Ram, pattern Bull, pattern Twins, pattern Crab, pattern Lion, pattern Maiden, pattern Scales, pattern Scorpius, pattern Scorpion
  , pattern Centaur, pattern Archer, pattern Capricornus, pattern MountainGoat, pattern GoatHorned, pattern SeaGoat, pattern WaterBearer
  , pattern Fish
    -- * Pattern synonyms for the 'SkinColorModifier' elements
  , pattern FitzPatrickI, pattern FitzPatrickII, pattern FitzPatrickIII, pattern FitzPatrickIV, pattern FitzPatrickV, pattern FitzPatrickVI
    -- * Submodule import
  , module Data.Char.Emoji.Flag
  ) where

import Control.DeepSeq(NFData)

import Data.Bits(Bits((.&.), (.|.), bit, bitSize, bitSizeMaybe, complement, isSigned, popCount, rotate, shift, shiftL, shiftR, testBit, xor))
import Data.Bool(bool)
import Data.Char(chr, ord)
import Data.Char.Core(MirrorVertical(mirrorVertical), UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar'), UnicodeText(fromUnicodeText, toUnicodeText), mapFromEnum, mapToEnum, mapToEnumSafe)
import Data.Char.Emoji.Flag
import Data.Data(Data)
import Data.Function(on)
import Data.Hashable(Hashable)
import Data.Text(unpack)

import GHC.Enum(toEnumError)
import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- | A 'Char'acter that is often used as a suffix to turn a character into an
-- emoji.
pattern EmojiSuffix :: Char
pattern EmojiSuffix = '\xfe0f'

_skinColorOffset :: Int
_skinColorOffset = 0x1f3fb

_zodiacOffset :: Int
_zodiacOffset = 0x2648

_moonPhaseOffset :: Int
_moonPhaseOffset = 0x1f311

-- | A 'Clock' object that can be converted to a unicode character that displays
-- a clock with the given time. The 'Clock' has an 'hours' field that contains
-- the given hours between 0 and 12, and a 'minutes30' field that if 'True',
-- means that the clock is half past that hour.
data Clock = Clock {
    hours :: Int  -- ^ The number of hours on the given clock. Is between 0 and 12. For 0, the 'minutes30' is 'True'; and for 12, the 'minutes30' is 'False'.
  , minutes30 :: Bool  -- ^ Is 'True' if it is half past the given hour on the 'Clock'.
  } deriving (Data, Eq, Generic, Ord, Read, Show)

instance Hashable Clock

instance NFData Clock

-- | A 'BloodType' object used to convert to its unicode equivalent. The
-- 'BloodType' is also seen as a 2-bit value with the leftmost bit representing
-- the presence of /A antigens/ and the rightmost the presence of /B antigens/.
data BloodType
  = O  -- ^ The /O blood type/, with no presence of A and B antigens.
  | B  -- ^ The /B blood type/, with presence of the B antigen.
  | A  -- ^ The /A blood type/, with presence of the A antigen.
  | AB  -- ^ The /AB blood type/, with presence of the A and B antigens.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Arbitrary BloodType where
    arbitrary = arbitraryBoundedEnum

instance Hashable BloodType

instance NFData BloodType

_overEnumMask :: Enum a => Int -> (Int -> Int) -> a -> a
_overEnumMask m f = toEnum . (m .&.) . f . fromEnum

_overEnum2 :: Enum a => (Int -> Int -> Int) -> a -> a -> a
_overEnum2 f x y = toEnum (on f fromEnum x y)

_overEnumMask2 :: Enum a => Int -> (Int -> Int -> Int) -> a -> a -> a
_overEnumMask2 m f x y = toEnum (m .&. on f fromEnum x y)

instance Bits BloodType where
    (.&.) = _overEnum2 (.&.)
    (.|.) = _overEnum2 (.|.)
    xor = _overEnumMask2 0x03 xor
    complement O = AB
    complement A = B
    complement B = A
    complement AB = O
    shift abo n = _overEnumMask 0x03 (`shift` n) abo
    rotate = flip (go . (0x01 .&.))
        where go 1 A = B
              go 1 B = B
              go _ x = x
    bitSize = const 2
    bitSizeMaybe = const (Just 2)
    isSigned = const False
    testBit = testBit . fromEnum
    bit 0 = B
    bit 1 = A
    bit _ = O
    popCount O = 0
    popCount A = 1
    popCount B = 1
    popCount AB = 2

instance Bounded Clock where
    minBound = Clock 0 True
    maxBound = Clock 12 False

instance Enum Clock where
    fromEnum (Clock h m30) = pred (shiftL h 1 .|. bool 0 1 m30)
    toEnum hm30
        | hm30 < 0 || hm30 > 23 = toEnumError "Clock" hm30 (minBound :: Clock, maxBound)
        | otherwise = Clock (shiftR hm30' 1) (odd hm30')
        where hm30' = succ hm30
    enumFrom = (`enumFromTo` maxBound)
    enumFromThen x y = enumFromThenTo x y maxBound

-- | Generate the 'Clock' object that is the closest to the given hours and
-- minutes.
closestClock
  :: Int  -- ^ The number of hours.
  -> Int  -- ^ The number of minutes, must be between 0 and 60.
  -> Clock  -- ^ The clock object that is the closest to the given hours and minutes.
closestClock h m
    | m < 15 = clock h False
    | m < 45 = clock h True
    | otherwise = clock (h+1) False

-- | Construct a 'Clock' object with the given number of hours, and a 'Bool'ean
-- that indicates if it is half past that hour.
-- The function will ensure that the hours are between 0 and 12 (both inclusive).
-- For half past 12, we use half past 0, for 12 hours, we use simply 12.
clock
  :: Int  -- ^ The given hour of the clock, can be any value, but will be set between 1 and 12.
  -> Bool  -- ^ A 'Bool'ean that indicates if it is half past that hour, so 'True' means we add 30 minutes.
  -> Clock  -- ^ A clock object that represents the time that is passed through an hour and .
clock h b
    | b && h' == 12 = Clock 0 True
    | otherwise = Clock h' b
    where h' = mod (h-1) 12 + 1

-- | A data type to specify the /gender/ of a person, animal, etc. used in an
-- emoji. The 'Gender' items are an instance of 'UnicodeText' that maps to the
-- /female/ and /male/ emoji.
data Gender
  = Female -- The female sign, dented by ♀️.
  | Male  -- The male sign, denoted by ♂️.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable Gender

instance NFData Gender

-- | Some emoji deal with people. One can change the color of the skin with the
-- 'SkinColorModifier'. For the skin color, the <https://en.wikipedia.org/wiki/Fitzpatrick_scale /Fitzpatrick scale/> is used.
-- A numerical classification system for skin types.
data SkinColorModifier
  = Light  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ one or two to the Emoji.
  | MediumLight  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ three to the Emoji.
  | Medium  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ four to the Emoji.
  | MediumDark  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ five to the Emoji.
  | Dark  -- ^ An emoji /modifier/ that applies /Fitzpatrick skin type/ six to the Emoji.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable SkinColorModifier

instance NFData SkinColorModifier

instance Arbitrary SkinColorModifier where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Gender where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Zodiac where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Clock where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary MoonPhase where
    arbitrary = arbitraryBoundedEnum

instance UnicodeCharacter SkinColorModifier where
    toUnicodeChar = mapFromEnum _skinColorOffset
    fromUnicodeChar = mapToEnumSafe _skinColorOffset
    fromUnicodeChar' = mapToEnum _skinColorOffset

instance UnicodeCharacter Zodiac where
    toUnicodeChar = mapFromEnum _zodiacOffset
    fromUnicodeChar = mapToEnumSafe _zodiacOffset
    fromUnicodeChar' = mapToEnum _zodiacOffset

instance UnicodeCharacter MoonPhase where
    toUnicodeChar = mapFromEnum _moonPhaseOffset
    fromUnicodeChar = mapToEnumSafe _moonPhaseOffset
    fromUnicodeChar' = mapToEnum _moonPhaseOffset

instance UnicodeCharacter Clock where
    toUnicodeChar (Clock h False) = chr (0x1f54f + h)
    toUnicodeChar (Clock h True) = chr (0x1f55c + mod (h-1) 12)
    fromUnicodeChar c
        | c < '\x1f550' = Nothing
        | c < '\x1f55c' = Just (Clock (ord c - 0x1f54f) False)
        | c < '\x1f568' = Just (Clock (mod (ord c - 0x1f55b) 12) True)
        | otherwise = Nothing


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

instance Hashable Zodiac

instance NFData Zodiac

-- | A data type that defines the eight different moon phases, and is an
-- instance of 'UnicodeCharacter' to convert these to the corresponding Unicode
-- character.
data MoonPhase
  = NewMoon  -- ^ The /new moon/, the first phase of the moon represented by 🌑.
  | WaxingCrescent  -- ^ The /waxing crescent/, the second phase of the moon represented by 🌒.
  | FirstQuarter  -- ^ The /first quarter/, the third phase of the moon represented by 🌓.
  | WaxingGibbous  -- ^ The /waxing gibbous/, the fourth phase of the moon represented by 🌔.
  | FullMoon  -- ^ The /full moon/, the fifth phase of the moon represented by 🌕.
  | WaningGibbous  -- ^ The /waning gibbous/, the sixth phase of the moon represented by 🌖.
  | ThirdQuarter  -- ^ The /third quarter/, the seventh phase of the moon represented by 🌗.
  | WaningCrescent  -- ^ The /waning crescent/, the eighth phase of the moon represented by 🌘.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

instance Hashable MoonPhase

instance MirrorVertical MoonPhase where
  mirrorVertical = toEnum . (`mod` 8) . (8 -) . fromEnum

instance NFData MoonPhase

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

instance UnicodeText SkinColorModifier
instance UnicodeText Zodiac
instance UnicodeText MoonPhase
instance UnicodeText Clock

instance UnicodeText Gender where
    toUnicodeText Male = "\x2640\xfe0f"
    toUnicodeText Female = "\x2642\xfe0f"
    fromUnicodeText "\x2640\xfe0f" = Just Male
    fromUnicodeText "\x2642\xfe0f" = Just Female
    fromUnicodeText _ = Nothing

instance UnicodeText BloodType where
    toUnicodeText AB = "\x1f18e"
    toUnicodeText A = "\x1f170\xfe0f"
    toUnicodeText B = "\x1f171\xfe0f"
    toUnicodeText O = "\x1f17e\xfe0f"
    fromUnicodeText "\x1f18e" = Just AB
    fromUnicodeText t
        | [c, EmojiSuffix] <- unpack t = go c
        | otherwise = Nothing
        where go '\x1f170' = Just A
              go '\x1f171' = Just B
              go '\x1f17e' = Just O
              go _ = Nothing

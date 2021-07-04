{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings, Safe #-}

{-|
Module      : Data.Char.Emoji.Clock
Description : A module that defines emoji that display a clock.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

The emoji have 24 clock emoji, each time with 30 minutes difference.
-}

module Data.Char.Emoji.Clock (
    -- * Clock emoji
    Clock, hours, minutes30, clock, closestClock
  ) where

import Control.DeepSeq(NFData)

import Data.Bits((.|.), shiftL, shiftR)
import Data.Bool(bool)
import Data.Char(chr, ord)
import Data.Char.Core(UnicodeCharacter(toUnicodeChar, fromUnicodeChar), UnicodeText)
import Data.Data(Data)
import Data.Hashable(Hashable)

import GHC.Enum(toEnumError)
import GHC.Generics(Generic)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

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

instance Arbitrary Clock where
    arbitrary = arbitraryBoundedEnum

instance UnicodeCharacter Clock where
    toUnicodeChar (Clock h False) = chr (0x1f54f + h)
    toUnicodeChar (Clock h True) = chr (0x1f55c + mod (h-1) 12)
    fromUnicodeChar c
        | c < '\x1f550' = Nothing
        | c < '\x1f55c' = Just (Clock (ord c - 0x1f54f) False)
        | c < '\x1f568' = Just (Clock (mod (ord c - 0x1f55b) 12) True)
        | otherwise = Nothing


instance UnicodeText Clock

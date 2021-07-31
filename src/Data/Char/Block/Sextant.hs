{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, DeriveTraversable, FlexibleInstances, PatternSynonyms, Safe, TypeApplications #-}

{-|
Module      : Data.Char.Block.Sextant
Description : A module used to render blocks divided in three horizontal rows in unicode.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has 3-by-2 blocks, this module aims to make it more convenient to render such blocks.
-}

module Data.Char.Block.Sextant(
    -- * Datastructures to store the state of the sextant.
    Sextant(Sextant, upper, middle, lower), isSextant
    -- * A unicode character that is (partially) filled sextant.
  , filled
    -- * Convert a 'Char'acter to a (partially) filled sextant.
  , fromSextant, fromSextant'
  ) where

import Control.DeepSeq(NFData, NFData1)

import Data.Bits((.|.), (.&.), shiftL, shiftR)
import Data.Char(chr, ord)
import Data.Char.Core(MirrorHorizontal(mirrorHorizontal), MirrorVertical(mirrorVertical), UnicodeCharacter(toUnicodeChar, fromUnicodeChar, fromUnicodeChar', isInCharRange), UnicodeText(isInTextRange), generateIsInTextRange')
import Data.Char.Block(Row, pattern EmptyRow, pattern LeftRow, pattern RightRow, pattern FullRow, pattern EmptyBlock, pattern LeftHalfBlock, pattern RightHalfBlock, pattern FullBlock, rowValue, toRow')
import Data.Data(Data)
import Data.Functor.Classes(Eq1(liftEq), Ord1(liftCompare))
import Data.Hashable(Hashable)
import Data.Hashable.Lifted(Hashable1)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif

import GHC.Generics(Generic, Generic1)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)


-- | A data type that determines the state of the four subparts of the block.
data Sextant a = Sextant {
    upper :: Row a  -- ^ The upper part of the sextant.
  , middle :: Row a  -- ^ The middle part of the sextant.
  , lower :: Row a  -- ^ The lower part of the sextant.
  } deriving (Bounded, Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)


instance Eq1 Sextant where
  liftEq cmp ~(Sextant ua ma la) ~(Sextant ub mb lb) = cmp' ua ub && cmp' ma mb && cmp' la lb
    where cmp' = liftEq cmp

instance Hashable a => Hashable (Sextant a)

instance Hashable1 Sextant

instance MirrorVertical (Sextant a) where
  mirrorVertical (Sextant u m d) = Sextant (mirrorVertical u) (mirrorVertical m) (mirrorVertical d)

instance MirrorHorizontal (Sextant a) where
  mirrorHorizontal (Sextant u m d) = Sextant d m u

instance NFData a => NFData (Sextant a)

instance NFData1 Sextant

instance Ord1 Sextant where
  liftCompare cmp ~(Sextant ua ma la) ~(Sextant ub mb lb) = cmp' ua ub <> cmp' ma mb <> cmp' la lb
    where cmp' = liftCompare cmp

instance Applicative Sextant where
    pure x = Sextant px px px
      where px = pure x
    Sextant fu fm fl <*> Sextant u m l = Sextant (fu <*> u) (fm <*> m) (fl <*> l)

instance Arbitrary a => Arbitrary (Sextant a) where
    arbitrary = arbitrary1

instance Arbitrary1 Sextant where
    liftArbitrary arb = Sextant <$> arb' <*> arb' <*> arb'
      where arb' = liftArbitrary arb

instance UnicodeCharacter (Sextant Bool) where
    toUnicodeChar = filled
    fromUnicodeChar = fromSextant
    fromUnicodeChar' = fromSextant'
    isInCharRange = isSextant

instance UnicodeText (Sextant Bool) where
  isInTextRange = generateIsInTextRange' @(Sextant Bool)

-- | Check if the given 'Char'acter is a 'Char'acter that maps on a 'Sextant' value.
isSextant
  :: Char  -- ^ The given 'Char'acter to test.
  -> Bool  -- ^ 'True' if the given 'Char'acter is a /sextant/ 'Char'acter; otherwise 'False'.
isSextant ci = c1 || c2
  where c1 = '\x1FB00' <= ci && ci <= '\x1fb3b'
        c2 = ci `elem` [EmptyBlock, LeftHalfBlock, RightHalfBlock, FullBlock]

-- | Convert the given 'Char' to the corresponding 'Sextant' object wrapped
-- in a 'Just' data constructor. If the given 'Char' is not a sextant character,
-- 'Nothing' is returned.
fromSextant
  :: Char  -- ^ The 'Char' we wish to convert to a 'Sextant' object.
  -> Maybe (Sextant Bool)  -- ^ The corresponding 'Sextant' object wrapped in a 'Just'; 'Nothing' if the given 'Char' is not a sextant character.
fromSextant ci
  | isSextant ci = Just (fromSextant' ci)
  | otherwise = Nothing

-- | Convert the given 'Char' to the corresponding 'Sextant' object wrapped
-- If the given 'Char' is not a sextant character, it is unspecified what
-- will happen.
fromSextant'
  :: Char  -- ^ The 'Char' we wish to convert to a 'Sextant' object.
  -> Sextant Bool  -- ^ The corresponding 'Sextant'; unspecified behavior if the given 'Char' is not a sextant character.
fromSextant' EmptyBlock = Sextant EmptyRow EmptyRow EmptyRow
fromSextant' LeftHalfBlock = Sextant LeftRow LeftRow LeftRow
fromSextant' RightHalfBlock = Sextant RightRow RightRow RightRow
fromSextant' FullBlock = Sextant FullRow FullRow FullRow
fromSextant' ch = Sextant u m l
  where ci = ord ch .&. 0x3f
        ch'
          | ci >= 0x28 = ci + 3
          | ci > 0x13 = ci + 2
          | otherwise = ci + 1
        u = toRow' (ch' .&. 3)
        m = toRow' (shiftR ch' 2 .&. 3)
        l = toRow' (shiftR ch' 4)

-- | Convert the given 'Sextant' of 'Bool's to a 'Char' where raster items of the 'Sextant'
-- are written in black, and the rest in white.
filled
  :: Sextant Bool  -- ^ The given 'Sextant' of 'Bool's to convert to a 'Char'.
  -> Char  -- ^ The corresponding 'Char'acter that presents the sextant.
filled (Sextant u m d) = go (shiftL (rowValue d) 4 .|. shiftL (rowValue m) 2 .|. rowValue u)
  where go 0x00 = EmptyBlock
        go 0x15 = LeftHalfBlock
        go 0x2a = RightHalfBlock
        go 0x3f = FullBlock
        go i
          | i >= 0x2a = chr (0x1fb00 .|. (i-0x03))
          | i >= 0x15 = chr (0x1fb00 .|. (i-0x02))
          | otherwise = chr (0x1fb00 .|. (i-0x01))

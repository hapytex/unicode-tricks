{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, DeriveTraversable, FlexibleInstances, PatternSynonyms, Safe, TypeApplications #-}

{-|
Module      : Data.Char.Block
Description : A module used to render blocks in unicode.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

Unicode has 2-by-2 blocks, this module aims to make it more convenient to render such blocks.
-}

module Data.Char.Block(
    -- * Datastructures to store the state of the frame.
    Row(Row, left, right), rowValue, toRow, toRow'
  , pattern EmptyRow, pattern FullRow, pattern LeftRow, pattern RightRow
  , Block(Block, upper, lower)
    -- * A unicode character that is (partially) filled block.
  , filled
    -- * Convert a 'Char'acter to a (partially) filled block.
  , fromBlock, fromBlock'
    -- * Pattern synonyms for blocks
  , pattern EmptyBlock, pattern FullBlock, pattern LeftHalfBlock, pattern RightHalfBlock
  ) where

import Control.DeepSeq(NFData, NFData1)

import Data.Bits((.|.))
import Data.Bool(bool)
import Data.Char.Core(MirrorHorizontal(mirrorHorizontal), MirrorVertical(mirrorVertical), UnicodeCharacter(toUnicodeChar, fromUnicodeChar, isInCharRange), UnicodeText(isInTextRange), generateIsInTextRange')
import Data.Data(Data)
import Data.Functor.Classes(Eq1(liftEq), Ord1(liftCompare))
import Data.Hashable(Hashable)
import Data.Hashable.Lifted(Hashable1)
import Data.Maybe(fromJust)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif

import GHC.Generics(Generic, Generic1)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

-- | A pattern synonym for the /block/ 'Char'acter that will render a full block.
pattern FullBlock :: Char
pattern FullBlock = '\x2588'

-- | A pattern synonym for a /block/ 'Char'acter that will render an empty block, this is equivalent to a space.
pattern EmptyBlock :: Char
pattern EmptyBlock = ' '

-- | A pattern synonym for a /block/ 'Char'acter that will render a block where the /left/ half of the block is filled.
pattern LeftHalfBlock :: Char
pattern LeftHalfBlock = '\x258c'

-- | A pattern synonym for a /block/ 'Char'acter that will render a block where the /right/ half of the block is filled.
pattern RightHalfBlock :: Char
pattern RightHalfBlock = '\x2590'


-- | A data type that determines the state of the /row/ in a block.
-- it determines the left and the right part of the row of the block.
data Row a = Row {
    left :: a  -- ^ The left part of a row of the block.
  , right :: a  -- ^ The right part of the row of the block.
  } deriving (Bounded, Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

-- | A pattern synonym for a 'Row' where both the left and right subpart are 'True'.
pattern FullRow :: Row Bool
pattern FullRow = Row True True

-- | A pattern synonym for a 'Row' where both the left and right subpart are 'False'.
pattern EmptyRow :: Row Bool
pattern EmptyRow = Row False False

-- | A pattern synonym for a 'Row' where the left part is set to 'True', and the right part is set to 'False'.
pattern LeftRow :: Row Bool
pattern LeftRow = Row True False

-- | A pattern synonym for a 'Row' where the left part is set to 'False', and the right part is set to 'True'.
pattern RightRow :: Row Bool
pattern RightRow = Row False True

-- | Convert the given 'Row' of 'Bool'eans to an 'Int' where the left 'Bool' has value 1, and the right one has value two. The four different 'Row's thus are mapped to integers from zero to three (both inclusive).
rowValue
  :: Row Bool  -- ^ The given 'Row' of 'Bool's to convert.
  -> Int  -- ^ The corresponding numerical value.
rowValue ~(Row l r) = b0 1 l .|. b0 2 r
  where b0 = bool 0

-- | Convert the given number to a 'Row' of 'Bool's. If the value
-- is out of bounds, it is unspecified what will happen.
toRow'
  :: Int  -- ^ The given number to convert.
  -> Row Bool  -- ^ The corresponding 'Row' of 'Bool's.
toRow' i = Row (odd i) (i >= 0x02)

-- | Convert the given number to a 'Row' of 'Bool's wrapped in a 'Just'.
-- if the value is out of bounds, 'Nothing' is returned.
toRow
  :: Int -- ^ The given number to convert.
  -> Maybe (Row Bool)  -- ^ The corresponding 'Row' of 'Bool's.
toRow i
  | i >= 0x00 && i <= 0x03 = Just (toRow' i)
  | otherwise = Nothing

instance Eq1 Row where
  liftEq cmp ~(Row xa xb) ~(Row ya yb) = cmp xa ya && cmp xb yb

instance Hashable1 Row

instance Hashable a => Hashable (Row a)

instance MirrorVertical (Row a) where
  mirrorVertical (Row l r) = Row r l

instance NFData a => NFData (Row a)

instance NFData1 Row

instance Ord1 Row where
  liftCompare cmp ~(Row xa xb) ~(Row ya yb) = cmp xa ya <> cmp xb yb

-- | A data type that determines the state of the four subparts of the block.
data Block a = Block {
    upper :: Row a  -- ^ The upper part of the block.
  , lower :: Row a  -- ^ The lower part of the block.
  } deriving (Bounded, Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

instance Eq1 Block where
  liftEq cmp ~(Block ua la) ~(Block ub lb) = cmp' ua ub && cmp' la lb
    where cmp' = liftEq cmp

instance Hashable a => Hashable (Block a)

instance Hashable1 Block

instance MirrorVertical (Block a) where
  mirrorVertical (Block u d) = Block (mirrorVertical u) (mirrorVertical d)

instance MirrorHorizontal (Block a) where
  mirrorHorizontal (Block u d) = Block d u

instance NFData a => NFData (Block a)

instance NFData1 Block

instance Ord1 Block where
  liftCompare cmp ~(Block ua la) ~(Block ub lb) = cmp' ua ub <> cmp' la lb
    where cmp' = liftCompare cmp

instance Applicative Row where
    pure x = Row x x
    Row fl fr <*> Row l r = Row (fl l) (fr r)

instance Applicative Block where
    pure x = Block px px
      where px = pure x
    Block fu fl <*> Block u l = Block (fu <*> u) (fl <*> l)

instance Arbitrary a => Arbitrary (Row a) where
    arbitrary = arbitrary1

instance Arbitrary1 Row where
    liftArbitrary arb = Row <$> arb <*> arb

instance Arbitrary a => Arbitrary (Block a) where
    arbitrary = arbitrary1

instance Arbitrary1 Block where
    liftArbitrary arb = Block <$> arb' <*> arb'
        where arb' = liftArbitrary arb

-- | Convert the given 'Char'acter to a 'Block' of 'Bool's wrapped in a 'Just'
-- if it exists; 'Nothing' otherwise.
fromBlock
  :: Char  -- ^ The given 'Char'acter to convert to a 'Block' of 'Bool's.
  -> Maybe (Block Bool)  -- The equivalent 'Block' of 'Bool's wrapped in a 'Just' if such block exists; 'Nothing' otherwise.
fromBlock EmptyBlock = Just (Block (Row False False) (Row False False))
fromBlock '\x2580' = Just (Block (Row True  True ) (Row False False))
fromBlock '\x2584' = Just (Block (Row False False) (Row True  True ))
fromBlock FullBlock = Just (Block (Row True  True ) (Row True  True ))
fromBlock LeftHalfBlock = Just (Block (Row True  False) (Row True  False))
fromBlock RightHalfBlock = Just (Block (Row False True ) (Row False True ))
fromBlock '\x2596' = Just (Block (Row False False) (Row True  False))
fromBlock '\x2597' = Just (Block (Row False False) (Row False True ))
fromBlock '\x2598' = Just (Block (Row True  False) (Row False False))
fromBlock '\x2599' = Just (Block (Row True  False) (Row True  True ))
fromBlock '\x259a' = Just (Block (Row True  False) (Row False True ))
fromBlock '\x259b' = Just (Block (Row True  True ) (Row True  False))
fromBlock '\x259c' = Just (Block (Row True  True ) (Row False True ))
fromBlock '\x259d' = Just (Block (Row False True ) (Row False False))
fromBlock '\x259e' = Just (Block (Row False True ) (Row True  False))
fromBlock '\x259f' = Just (Block (Row False True ) (Row True  True ))
fromBlock _ = Nothing

-- | Convert the given 'Char'acter to a 'Block' of 'Bool's if it exists; unspecified result otherwise.
fromBlock'
  :: Char  -- ^ The given 'Char'acter to convert to a 'Block' of 'Bool's.
  -> Block Bool  -- ^ The equivalent 'Block' of 'Bool's.
fromBlock' = fromJust . fromBlock

-- | Convert the given 'Block' value to a block character in unicode.
-- 'True' means that part is filled, and 'False' means the part is not filled.
filled
    :: Block Bool  -- ^ The given 'Block' of 'Bool's to convert to a 'Char'acter.
    -> Char  -- ^ The equivalent Unicode 'Char'acter for the given 'Block' of 'Bool's.
filled (Block (Row False False) (Row False False)) = EmptyBlock
filled (Block (Row True  True ) (Row False False)) = '\x2580'
filled (Block (Row False False) (Row True  True )) = '\x2584'
filled (Block (Row True  True ) (Row True  True )) = FullBlock
filled (Block (Row True  False) (Row True  False)) = LeftHalfBlock
filled (Block (Row False True ) (Row False True )) = RightHalfBlock
filled (Block (Row False False) (Row True  False)) = '\x2596'
filled (Block (Row False False) (Row False True )) = '\x2597'
filled (Block (Row True  False) (Row False False)) = '\x2598'
filled (Block (Row True  False) (Row True  True )) = '\x2599'
filled (Block (Row True  False) (Row False True )) = '\x259a'
filled (Block (Row True  True ) (Row True  False)) = '\x259b'
filled (Block (Row True  True ) (Row False True )) = '\x259c'
filled (Block (Row False True ) (Row False False)) = '\x259d'
filled (Block (Row False True ) (Row True  False)) = '\x259e'
filled (Block (Row False True ) (Row True  True )) = '\x259f'

instance UnicodeCharacter (Block Bool) where
    toUnicodeChar = filled
    fromUnicodeChar = fromBlock
    isInCharRange c = ('\x2596' <= c && c <= '\x259f') || c `elem` " \x2588\x258c\x2590"

instance UnicodeText (Block Bool) where
    isInTextRange = generateIsInTextRange' @(Block Bool)

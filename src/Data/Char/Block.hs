{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, Safe #-}

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
    Row(Row, left, right)
  , Block(Block, upper, lower)
    -- * A unicode character that is (partially) filled block.
  , filled
  ) where

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary))


-- | A data type that determines the state of the /row/ in a block.
-- it determines the left and the right part of the row of the block.
data Row a = Row { 
    left :: a -- ^ The left part of a row of the block.
  , right :: a -- ^ The right part of the row of the block.
  } deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

-- | A data type that determines the state of the four subparts of the block.
data Block a = Block {
    upper :: Row a -- ^ The upper part of the block.
  , lower :: Row a -- ^ The lower part of the block.
  } deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

instance Applicative Row where
    pure x = Row x x
    Row fl fr <*> Row l r = Row (fl l) (fr r)

instance Applicative Block where
    pure x = Block (pure x) (pure x)
    Block fu fl <*> Block u l = Block (fu <*> u) (fl <*> l)

instance Arbitrary a => Arbitrary (Row a) where
    arbitrary = Row <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Block a) where
    arbitrary = Block <$> arbitrary <*> arbitrary

-- | Convert the given 'Block' value to a block character in unicode.
-- 'True' means that part is filled, and 'False' means the part is not filled.
filled
    :: Block Bool
    -> Char
filled (Block (Row False False) (Row False False)) = ' '
filled (Block (Row True  True ) (Row False False)) = '\x2580'
filled (Block (Row False False) (Row True  True )) = '\x2584'
filled (Block (Row True  True ) (Row True  True )) = '\x2588'
filled (Block (Row True  False) (Row True  False)) = '\x258c'
filled (Block (Row False True ) (Row False True )) = '\x2590'
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

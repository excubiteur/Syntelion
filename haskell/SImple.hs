{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StarIsType #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}

import Data.Bits
import Data.Word

-- --------------------------------------------
-- Set / OrderedSet type classes (from before)
-- --------------------------------------------
class (Eq t, Enum t, Bounded t) => Set t where
  cardinality :: Int
  cardinality = fromEnum (maxBound @t) - fromEnum (minBound @t) + 1

class (Ord t, Set t) => OrderedSet t

instance (Eq t, Enum t, Bounded t) => Set t
instance (Ord t, Eq t, Enum t, Bounded t) => OrderedSet t

-- --------------------------------------------
-- Subset type: a bitset of size 'cardinality'
-- --------------------------------------------
newtype Subset (s :: *) = Subset Word
  deriving (Eq, Bits, Show)

-- Create a subset from a list of elements
fromList :: forall s. Set s => [s] -> Subset s
fromList xs = Subset $ foldr (\x acc -> acc .|. bit (fromEnum x)) 0 xs

-- Convert a subset back to a list
toList :: forall s. Set s => Subset s -> [s]
toList (Subset w) = [ toEnum i | i <- [0..cardinality @s - 1], testBit w i ]

-- Example sets
data SomeSet = A1 | A2 | A3
  deriving (Eq, Show, Enum, Bounded)

data AnotherSet = B1 | B2 | B3 | B4
  deriving (Eq, Ord, Show, Enum, Bounded)

-- --------------------------------------------
-- Example usage
-- --------------------------------------------
example1 :: Subset SomeSet
example1 = fromList [A1, A3]    -- bitset representing {A1, A3}

example2 :: Subset AnotherSet
example2 = fromList [B2, B4]    -- bitset representing {B2, B4}

example1List :: [SomeSet]
example1List = toList example1  -- [A1, A3]

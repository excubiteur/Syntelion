{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

import Data.Bits (Bits)

class (Eq t, Enum t, Bounded t) => Set t where
  cardinality :: Int
  cardinality = fromEnum (maxBound @t) - fromEnum (minBound @t) + 1

class (Ord t, Set t) => OrderedSet t

-- set
instance (Eq t, Enum t, Bounded t) => Set t

-- ordered
instance (Ord t, Eq t, Enum t, Bounded t) => OrderedSet t

-- SOME_SET: set;
-- SOME_SUBSET : SOME_SET
newtype Subset (s :: *) = Subset Word
  deriving (Eq, Bits, Show)

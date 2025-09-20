{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Bits
import Data.Proxy
import Text.Printf

-- === Base classes ===

-- | Any Enum + Bounded type is a finite set.
class (Enum t, Bounded t) => Set t where
  cardinality :: proxy t -> Int
  cardinality _ = fromEnum (maxBound @t) + 1

  universe :: [t]
  universe = [minBound .. maxBound]

-- | Ordered sets are finite sets with a natural ordering.
class (Set t, Ord t) => Ordered t

-- === Subset type ===

newtype Subset t = Subset Integer
  deriving (Eq, Bits)

instance Set t => Show (Subset t) where
  show (Subset i) = "0b" ++ printf "%0*b" (cardinality (Proxy @t)) i

subset :: Integer -> Subset t
subset = Subset

-- === Bitwise operations ===

union, intersection :: Subset t -> Subset t -> Subset t
union = (.|.)
intersection = (.&.)

complementSubset :: forall t. Set t => Subset t -> Subset t
complementSubset (Subset i) = Subset (i `xor` mask)
  where
    mask = (1 `shiftL` cardinality (Proxy @t)) - 1

-- === Powerset generation ===

powerset :: forall t. Set t => [Subset t]
powerset = [ Subset i | i <- [0 .. maxVal] ]
  where
    maxVal = 2 ^ cardinality (Proxy @t) - 1

-- === Conversion between Subset and list of elements ===

subsetToList :: forall t. Set t => Subset t -> [t]
subsetToList (Subset i) = [ x | (x, idx) <- zip universe [0..], testBit i idx ]

listToSubset :: forall t. Set t => [t] -> Subset t
listToSubset xs = Subset $ foldr (\x acc -> setBit acc (fromEnum x)) 0 xs

-- === Example DSL outputs ===

-- From "SomeSet: set;"
data SomeSet = A0 | A1 | A2 | A3 | A4
  deriving (Eq, Show, Enum, Bounded)

instance Set SomeSet

-- From "AnotherSet: ordered;"
data AnotherSet = B0 | B1 | B2
  deriving (Eq, Show, Enum, Bounded, Ord)

instance Set AnotherSet
instance Ordered AnotherSet

-- === Demo ===

s1 :: Subset SomeSet
s1 = listToSubset [A0, A1]

s2 :: Subset SomeSet
s2 = listToSubset [A0]

allSubsets :: [Subset SomeSet]
allSubsets = powerset @SomeSet

main :: IO ()
main = do
  putStrLn "Example subsets:"
  print s1
  print s2

  putStrLn "\nUnion of s1 and s2:"
  print (s1 `union` s2)

  putStrLn "\nIntersection of s1 and s2:"
  print (s1 `intersection` s2)

  putStrLn "\nComplement of s1:"
  print (complementSubset s1 :: Subset SomeSet)

  putStrLn "\nSubset s1 as list of elements:"
  print (subsetToList s1)

  putStrLn "\nAll subsets of SomeSet (first 16 shown):"
  mapM_ print (take 16 allSubsets)

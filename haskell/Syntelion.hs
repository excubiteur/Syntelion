{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Bits
import Data.Proxy
import Text.Printf

-- === Type Classes ===

-- Any finite set type that is Enum + Bounded automatically gets cardinality
class (Eq t, Enum t, Bounded t) => Set t where
  cardinality :: Int
  cardinality = fromEnum (maxBound :: t) + 1

class (Ord t, Set t) => Ordered t where

-- === Subset type ===

newtype Subset t = Subset Integer
  deriving (Eq, Bits)

instance Set t => Show (Subset t) where
  show (Subset i) = "0b" ++ printf "%0*b" width i
    where
      width = cardinality @t

subset :: Integer -> Subset t
subset = Subset

-- === Bitwise operations ===

union :: Subset t -> Subset t -> Subset t
union = (.|.)

intersection :: Subset t -> Subset t -> Subset t
intersection = (.&.)

complementSubset :: forall t. Set t => Subset t -> Subset t
complementSubset (Subset i) = Subset (i `xor` mask)
  where
    mask = (1 `shiftL` cardinality @t) - 1

-- === Powerset generation ===

powerset :: forall t. Set t => [Subset t]
powerset = [ Subset i | i <- [0 .. maxVal] ]
  where
    maxVal = 2 ^ cardinality @t - 1

-- === Conversion between Subset and list of elements ===

subsetToList :: forall t. (Set t, Enum t, Bounded t) => Subset t -> [t]
subsetToList (Subset i) = [ x | (x, idx) <- zip universe [0..], testBit i idx ]
  where
    universe = [minBound .. maxBound]

listToSubset :: forall t. (Set t, Enum t, Bounded t) => [t] -> Subset t
listToSubset xs = Subset $ foldr (\x acc -> setBit acc (fromEnum x)) 0 xs

-- === Example set with minimal boilerplate ===

data SomeSet = A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9
  deriving (Eq, Show, Enum, Bounded, Ord)

instance Set SomeSet  -- cardinality auto-derived

-- === Example usage ===

s1 :: Subset SomeSet
s1 = listToSubset [A0, A1]  -- 0b0000000011

s2 :: Subset SomeSet
s2 = listToSubset [A0]      -- 0b0000000001

allSubsets :: [Subset SomeSet]
allSubsets = powerset @SomeSet

-- === Demo main ===

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

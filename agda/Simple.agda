{-# OPTIONS --safe #-}

module SubsetBitVector where

open import Data.Nat
open import Data.Fin
open import Data.List
open import Data.Bool
open import Relation.Binary.PropositionalEquality
open import Data.Maybe

-- --------------------------------------------------
-- Finite set type
-- --------------------------------------------------
record Set : Set1 where
  field
    Carrier     : Set
    cardinality : ℕ
    enum        : List Carrier

-- --------------------------------------------------
-- Ordered set: wraps a Set with a comparison
-- --------------------------------------------------
record OrderedSet : Set1 where
  field
    base    : Set
    compare : base.Carrier → base.Carrier → Bool
    -- compare x y = true if x ≤ y, false otherwise

-- --------------------------------------------------
-- Subset as a bitvector (ℕ used as bits)
-- --------------------------------------------------
Subset : Set → Set
Subset s = ℕ  -- bits 0..cardinality-1 represent presence of elements

-- --------------------------------------------------
-- Convert element to bit index
-- --------------------------------------------------
indexOf : {s : Set} → s.Carrier → Fin s.cardinality
indexOf x = indexOfAux x (Set.enum s) 0
  where
    indexOfAux : s.Carrier → List s.Carrier → ℕ → Fin s.cardinality
    indexOfAux x [] n = zero  -- fallback; assumes element exists
    indexOfAux x (y ∷ ys) n with x ≟ y
    ... | yes _ = n
    ... | no _  = indexOfAux x ys (n + 1)

-- --------------------------------------------------
-- Create a subset from a list of elements
-- --------------------------------------------------
fromList : {s : Set} → List s.Carrier → Subset s
fromList xs = foldl (λ acc x → acc Data.Bits..|. (bit (indexOf x))) 0 xs

-- --------------------------------------------------
-- Convert a subset back to a list
-- --------------------------------------------------
toList : {s : Set} → Subset s → List s.Carrier
toList w = filter (λ x → testBit w (indexOf x)) (Set.enum s)

-- --------------------------------------------------
-- Bit operations
-- --------------------------------------------------
bit : Fin n → ℕ
bit i = 2 ^ (toℕ i)

testBit : ℕ → Fin n → Bool
testBit w i = (w Data.Bits..&. (bit i)) /= 0

union : {s : Set} → Subset s → Subset s → Subset s
union x y = x Data.Bits..|. y

intersection : {s : Set} → Subset s → Subset s → Subset s
intersection x y = x Data.Bits..&. y

difference : {s : Set} → Subset s → Subset s → Subset s
difference x y = x Data.Bits..&. (Data.Bits.complement y)

-- --------------------------------------------------
-- Infix operators for Subset
-- --------------------------------------------------
infix 4 _∈_
_∈_ : {s : Set} → s.Carrier → Subset s → Bool
x ∈ subset = testBit subset (indexOf x)

infix 4 _⊆_
_⊆_ : {s : Set} → Subset s → Subset s → Bool
a ⊆ b = (a Data.Bits..&. b) == a

infixl 6 _∪_
_∪_ : {s : Set} → Subset s → Subset s → Subset s
_∪_ = union

infixl 7 _∩_
_∩_ : {s : Set} → Subset s → Subset s → Subset s
_∩_ = intersection

infixl 6 _∖_
_∖_ : {s : Set} → Subset s → Subset s → Subset s
_∖_ = difference

-- --------------------------------------------------
-- Example sets
-- --------------------------------------------------
data SomeSetElem : Set where
  A1 A2 A3 : SomeSetElem

SomeSet : Set
SomeSet = record
  { Carrier = SomeSetElem
  ; cardinality = 3
  ; enum = A1 ∷ A2 ∷ A3 ∷ []
  }

data AnotherSetElem : Set where
  B1 B2 B3 B4 : AnotherSetElem

AnotherSet : Set
AnotherSet = record
  { Carrier = AnotherSetElem
  ; cardinality = 4
  ; enum = B1 ∷ B2 ∷ B3 ∷ B4 ∷ []
  }

-- --------------------------------------------------
-- Example ordered set
-- --------------------------------------------------
SomeOrderedSet : OrderedSet
SomeOrderedSet = record
  { base = SomeSet
  ; compare = λ x y → case (x , y) of
      (A1 , A1) → true
      (A1 , A2) → true
      (A1 , A3) → true
      (A2 , A1) → false
      (A2 , A2) → true
      (A2 , A3) → true
      (A3 , A1) → false
      (A3 , A2) → false
      (A3 , A3) → true
  }

-- --------------------------------------------------
-- Example usage
-- --------------------------------------------------
example1 : Subset SomeSet
example1 = fromList [A1 , A3]

example2 : Subset AnotherSet
example2 = fromList [B2 , B4]

example1List : List SomeSetElem
example1List = toList example1

exampleUnionOp : Subset SomeSet
exampleUnionOp = example1 ∪ fromList [A2]

exampleIntersectionOp : Subset SomeSet
exampleIntersectionOp = example1 ∩ fromList [A2 , A3]

exampleDifferenceOp : Subset SomeSet
exampleDifferenceOp = example1 ∖ fromList [A3]

exampleMember1 : Bool
exampleMember1 = A1 ∈ example1  -- true

exampleMember2 : Bool
exampleMember2 = A2 ∈ example1  -- false

exampleSubsetCheck : Bool
exampleSubsetCheck = fromList [A1] ⊆ example1  -- true

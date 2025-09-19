{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Data.UnitsVector
  ( Quantity(..)
  , (+:), (*:), (/:)
  , Neg
  , Normalize, AddDims
  -- Examples
  , Length, Time, Velocity
  , Mass, USD, CoalDim, CoalGradeA, CoalGradeAWeight
  , CoalDimension, CoalPriceUSD
  ) where

import GHC.TypeLits
import Data.Kind (Type)

--------------------------------------------------------------------------------
-- Quantity type
--------------------------------------------------------------------------------

type Quantity :: [(Symbol, Int)] -> Type -> Type
newtype Quantity dims a = Q a
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Addition (only same dims)
--------------------------------------------------------------------------------

(+:) :: Num a => Quantity d a -> Quantity d a -> Quantity d a
(Q x) +: (Q y) = Q (x + y)

--------------------------------------------------------------------------------
-- Normalize dimensions
--------------------------------------------------------------------------------

-- Remove zeros
type family DropZero (ds :: [(Symbol, Int)]) :: [(Symbol, Int)] where
  DropZero '[] = '[]
  DropZero ('(k,0) ': ds) = DropZero ds
  DropZero (p ': ds) = p ': DropZero ds

-- Merge duplicate keys (assumes sorted)
type family Merge (ds :: [(Symbol,Int)]) :: [(Symbol,Int)] where
  Merge '[] = '[]
  Merge '[x] = '[x]
  Merge ('(k,v) ': '(k,v2) ': ds) = Merge ('(k,v+v2) ': ds)
  Merge (x ': ds) = x ': Merge ds

-- Insert-sort by Symbol
type family Insert (x :: (Symbol,Int)) (xs :: [(Symbol,Int)]) :: [(Symbol,Int)] where
  Insert '(k,v) '[] = '[ '(k,v) ]
  Insert '(k,v) ('(k2,v2) ': xs) =
    CmpInsert (CmpSymbol k k2) '(k,v) '(k2,v2) xs

type family CmpInsert (o :: Ordering) (x :: (Symbol,Int)) (y :: (Symbol,Int)) (ys :: [(Symbol,Int)]) :: [(Symbol,Int)] where
  CmpInsert 'LT x y ys = x ': y ': ys
  CmpInsert 'EQ '(k,v) '(k2,v2) ys = '(k,v+v2) ': ys
  CmpInsert 'GT x y ys = y ': Insert x ys

-- Full sort
type family Sort (xs :: [(Symbol,Int)]) :: [(Symbol,Int)] where
  Sort '[] = '[]
  Sort (x ': xs) = Insert x (Sort xs)

-- Normalize = sort + merge + drop zeros
type family Normalize (xs :: [(Symbol,Int)]) :: [(Symbol,Int)] where
  Normalize xs = DropZero (Merge (Sort xs))

--------------------------------------------------------------------------------
-- Exponent arithmetic
--------------------------------------------------------------------------------

type family Neg (ds :: [(Symbol, Int)]) :: [(Symbol, Int)] where
  Neg '[] = '[]
  Neg ('(k,v) ': ds) = '(k, NegInt v) ': Neg ds

type family NegInt (n :: Int) :: Int where
  NegInt n = (0 - n)

-- merge exponents then normalize
type family AddDims (d1 :: [(Symbol, Int)]) (d2 :: [(Symbol, Int)]) :: [(Symbol, Int)] where
  AddDims '[] d2 = Normalize d2
  AddDims d1 '[] = Normalize d1
  AddDims ( '(k,v) ': ds) d2 = AddDims ds (Insert '(k,v) d2)

--------------------------------------------------------------------------------
-- Multiplication & division
--------------------------------------------------------------------------------

(*:) :: Num a => Quantity d1 a -> Quantity d2 a -> Quantity (AddDims d1 d2) a
(Q x) *: (Q y) = Q (x * y)

(/:) :: Fractional a => Quantity d1 a -> Quantity d2 a -> Quantity (AddDims d1 (Neg d2)) a
(Q x) /: (Q y) = Q (x / y)

--------------------------------------------------------------------------------
-- Example dimensions
--------------------------------------------------------------------------------

type Length = Normalize '[ '("L", 1) ]
type Time   = Normalize '[ '("T", 1) ]
type Velocity = AddDims Length (Neg Time)

type Mass = Normalize '[ '("kg",1) ]
type USD = Normalize '[ '("USD",1) ]

-- Application-specific
type CoalDim = Normalize '[ '("Coal", 1) ]
type CoalGradeA = Normalize '[ '("CoalGradeA",1) ]
type CoalGradeAWeight = AddDims Mass CoalGradeA

--------------------------------------------------------------------------------
-- Dimension bijections (example)
--------------------------------------------------------------------------------

type family CoalDimension (g :: Symbol) :: [(Symbol,Int)] where
  CoalDimension "A" = '[ '("CoalGradeA",1) ]
  CoalDimension "B" = '[ '("CoalGradeB",1) ]

type CoalPriceUSD (g :: Symbol) = AddDims USD (Neg (CoalDimension g))

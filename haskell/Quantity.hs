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
  , AddDims
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
-- Addition
--------------------------------------------------------------------------------

(+:) :: Num a => Quantity d a -> Quantity d a -> Quantity d a
(Q x) +: (Q y) = Q (x + y)

--------------------------------------------------------------------------------
-- Type-level exponent arithmetic
--------------------------------------------------------------------------------

type family Neg (ds :: [(Symbol, Int)]) :: [(Symbol, Int)] where
  Neg '[] = '[]
  Neg ('(k,v) ': ds) = '(k, NegInt v) ': Neg ds

type family NegInt (n :: Int) :: Int where
  NegInt n = (0 - n)

-- merge exponents
type family AddDims (d1 :: [(Symbol, Int)]) (d2 :: [(Symbol, Int)]) :: [(Symbol, Int)] where
  AddDims '[] d2 = d2
  AddDims ('(k,v) ': ds) d2 = InsertDim k v (AddDims ds d2)

type family InsertDim (k :: Symbol) (v :: Int) (ds :: [(Symbol, Int)]) :: [(Symbol, Int)] where
  InsertDim k v '[] = '[ '(k,v) ]
  InsertDim k v ('(k,v2) ': ds) = IfZero (v+v2) ds '( '(k,v+v2) ': ds)
  InsertDim k v (p ': ds) = p ': InsertDim k v ds

type family IfZero (n :: Int) (ds :: [(Symbol,Int)]) (full :: [(Symbol,Int)]) :: [(Symbol,Int)] where
  IfZero 0 ds _ = ds
  IfZero _ _ full = full

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

type Length = '[ '("L", 1) ]
type Time   = '[ '("T", 1) ]
type Velocity = '[ '("L",1), '("T",-1) ]

type Mass = '[ '("kg",1) ]
type USD = '[ '("USD",1) ]

-- Application-specific
type CoalDim = '[ '("Coal", 1) ]
type CoalGradeA = '[ '("CoalGradeA",1) ]
type CoalGradeAWeight = AddDims Mass CoalGradeA

--------------------------------------------------------------------------------
-- Dimension bijections (example)
--------------------------------------------------------------------------------

type family CoalDimension (g :: Symbol) :: [(Symbol,Int)] where
  CoalDimension "A" = '[ '("CoalGradeA",1) ]
  CoalDimension "B" = '[ '("CoalGradeB",1) ]

type CoalPriceUSD (g :: Symbol) = AddDims USD (Neg (CoalDimension g))


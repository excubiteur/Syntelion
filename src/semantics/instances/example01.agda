open FiniteType

-- opaque type
data CITY : Set₀

-- instance of FiniteType for CITY
instance
    FiniteType CITY = record
        { elems    = []          -- fill later
        ; complete = λ x → {!!}  -- proof that x ∈ elems
        }

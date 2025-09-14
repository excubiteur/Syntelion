open import Agda.Builtin.List
open import Agda.Builtin.Equality

record FiniteType (A : Set₀) : Set₁ where
    field
        elems    : List A
        complete : (x : A) → x ∈ elems

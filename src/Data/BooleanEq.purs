module Data.BooleanEq where

import Prelude
import Data.BooleanAlgebra (tt, ff)

-- | Expresses a congruence with Boolean equality
-- |
-- | Instances must also satisfy:  
-- | `((a == tt) `xor` (a == ff)) == true`
class (BooleanAlgebra a, Eq a) ⇐ BooleanEq a

instance booleanBooleanEq ∷ BooleanEq Boolean

xor ∷ ∀ a. HeytingAlgebra a ⇒ a → a → a
xor p q = (p `disj` q) `conj` (not (p `conj` q))

infixr 2 xor as ⊕

isBooleanEq ∷ ∀ a. BooleanEq a ⇒ a → Boolean
isBooleanEq a = (a == tt) ⊕ (a == ff)

toBoolean ∷ ∀ a. BooleanEq a ⇒ a → Boolean
toBoolean a = if (a == tt) then true else false

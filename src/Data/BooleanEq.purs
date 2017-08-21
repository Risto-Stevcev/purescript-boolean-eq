module Data.BooleanEq where

import Prelude
import Data.BooleanAlgebra (tt, ff)

-- | Expresses a congruence with Boolean equality
-- |
-- | Instances must also satisfy:  
-- | `((a == tt) || (a == ff)) == true`
class (BooleanAlgebra a, Eq a) ⇐ BooleanEq a

instance booleanBooleanEq ∷ BooleanEq Boolean

isBooleanEq ∷ ∀ a. BooleanEq a ⇒ a → Boolean
isBooleanEq a = (a == tt) || (a == ff)

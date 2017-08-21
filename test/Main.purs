module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.Data.BooleanEq (main) as Test
import Test.Spec.QuickCheck (QCRunnerEffects)

main ∷ Eff (QCRunnerEffects (exception ∷ EXCEPTION)) Unit
main = Test.main

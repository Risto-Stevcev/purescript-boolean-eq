module Test.Data.BooleanEq where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.BooleanEq (class BooleanEq, isBooleanEq)
import Data.Either (isLeft)
import Data.HeytingAlgebra (tt, ff)
import Data.Newtype (class Newtype, over, over2)
import Data.NonEmpty ((:|))
import Test.QuickCheck (class Testable)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, elements)
import Test.QuickCheck.Laws.Data.BooleanAlgebra (checkBooleanAlgebra)
import Test.QuickCheck.Laws.Data.HeytingAlgebra (checkHeytingAlgebra)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Type.Proxy (Proxy(..))

quickCheckFail ∷ ∀ t e. Testable t ⇒ t → Aff (random ∷ RANDOM | e) Unit
quickCheckFail a = do
  result ← attempt $ quickCheck a
  isLeft result `shouldEqual` true



-- | Some four valued logic
-- | And example of something that's a BooleanAlgebra but not a BooleanEq
data Logic = T | A | B | F

logicAnd ∷ Logic → Logic → Logic
logicAnd F _ = F
logicAnd _ F = F
logicAnd A A = A
logicAnd B B = B
logicAnd T T = T
logicAnd A B = F
logicAnd A T = A
logicAnd B T = B
logicAnd a b = (flip logicAnd) a b

logicOr ∷ Logic → Logic → Logic
logicOr F a = a
logicOr T a = T
logicOr A A = A
logicOr B B = B
logicOr A B = T
logicOr a b = (flip logicOr) a b

logicNot ∷ Logic → Logic
logicNot T = F
logicNot A = B
logicNot B = A
logicNot F = T

-- | This Eq implementation doesn't satisfy BooleanEq
instance eqLogic ∷ Eq Logic where
  eq T T = true
  eq F F = true
  eq A A = true
  eq B B = true
  eq _ _ = false

instance arbitraryLogic ∷ Arbitrary Logic where
  arbitrary = elements $ T :| [A, B, F]

instance heytingAlgebraLogic ∷ HeytingAlgebra Logic where
  not = logicNot
  disj = logicOr
  conj = logicAnd
  implies a b = not a `disj` b
  tt = T
  ff = F

instance booleanAlgebraLogic ∷ BooleanAlgebra Logic

instance booleanEqLogic ∷ BooleanEq Logic


-- | Example logic that's isomorphic to Boolean
data Toggle = On | Off

instance arbitraryToggle ∷ Arbitrary Toggle where
  arbitrary = elements $ On :| [Off]

-- | This Eq implementation satisfies BooleanEq
instance eqToggle ∷ Eq Toggle where
  eq On On = true
  eq Off Off = true
  eq _ _ = false

instance heytingAlgebraToggle ∷ HeytingAlgebra Toggle where
  not On  = Off
  not Off = On

  disj On  On  = On
  disj Off Off = Off
  disj On  Off = On
  disj Off On  = On

  conj On  On  = On
  conj Off Off = Off
  conj On  Off = Off
  conj Off On  = Off

  implies a b = not a `disj` b

  tt = On
  ff = Off

instance booleanAlgebraToggle ∷ BooleanAlgebra Toggle

instance booleanEqToggle ∷ BooleanEq Toggle


-- | Example showing that Unit does not satisfy BooleanEq because `tt == ff`
newtype Foo = Foo Unit

derive instance newtypeFoo ∷ Newtype Foo _

instance heytingAlgebraFoo ∷ HeytingAlgebra Foo where
  not = over Foo not
  conj = over2 Foo conj
  disj = over2 Foo disj
  implies a b = not a `disj` b
  tt = Foo (tt ∷ Unit)
  ff = Foo (ff ∷ Unit)

instance booleanAlgebraFoo ∷ BooleanAlgebra Foo

instance eqFoo ∷ Eq Foo where
  eq (Foo a) (Foo b) = a == b

instance booleanEqFoo ∷ BooleanEq Foo

instance arbitraryFoo ∷ Arbitrary Foo where
  arbitrary = (pure <<< Foo) =<< (arbitrary ∷ Gen Unit)



main ∷ Eff (QCRunnerEffects (exception ∷ EXCEPTION)) Unit
main = run [consoleReporter] do
  describe "BooleanEq Boolean" do
    it "should satisfy the BooleanEq laws" do
      quickCheck (isBooleanEq ∷ Boolean → Boolean)

  describe "HeytingAlgebra Logic" do
    it "should satisfy the HeytingAlgebra laws" do
      liftEff $ checkHeytingAlgebra (Proxy ∷ Proxy Logic)
  describe "BooleanAlgebra Logic" do
    it "should satisfy the BooleanAlgebra laws" do
      liftEff $ checkBooleanAlgebra (Proxy ∷ Proxy Logic)
  describe "BooleanEq Logic" do
    it "should not satisfy the BooleanEq laws" do
      quickCheckFail (isBooleanEq ∷ Logic → Boolean)

  describe "HeytingAlgebra Toggle" do
    it "should satisfy the HeytingAlgebra laws" do
      liftEff $ checkHeytingAlgebra (Proxy ∷ Proxy Toggle)
  describe "BooleanAlgebra Toggle" do
    it "should satisfy the BooleanAlgebra laws" do
      liftEff $ checkBooleanAlgebra (Proxy ∷ Proxy Toggle)
  describe "BooleanEq Toggle" do
    it "should satisfy the BooleanEq laws" do
      quickCheck (isBooleanEq ∷ Toggle → Boolean)

  describe "BooleanEq Foo (Unit)" do
    it "should not satisfy the BooleanEq laws (tt == ff)" do
      quickCheckFail (isBooleanEq ∷ Foo → Boolean)

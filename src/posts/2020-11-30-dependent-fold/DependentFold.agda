open import Data.Nat
open import Data.Vec
open import Data.List

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

module DependentFold where

Nat = ℕ

dfold : ∀ {a : Set}
      → {k : Nat}
      → (p : Nat → Set)
      → ((l : Nat) → a → p l → p (1 + l))
      → p 0
      → Vec a k
      → p k
dfold {k = 0} p step base [] = base
dfold {k = suc n} p step base (x ∷ xs) = step n x (dfold p step base xs)

foldrₗ : {a b : Set} -> (a -> b -> b) -> b -> List a -> b
foldrₗ step base []       = base
foldrₗ step base (x ∷ xs) = step x (foldrₗ step base xs)

map_motive : Set -> Nat -> Set
map_motive a l = Vec a l

dmap : {a b : Set} → {n : ℕ} → (a → b) → Vec a n → Vec b n
dmap {a} {b} {n} f xs = dfold (map_motive b) (λ _ x xs → f x ∷ xs) [] xs

_ : dmap (λ x → x + 3) (1 ∷ 2 ∷ 3 ∷ []) ≡ (4 ∷ 5 ∷ 6 ∷ [])
_ = refl

dappend : {a : Set} → {m n : ℕ} → Vec a m → Vec a n → Vec a (m + n)
dappend {a} {m} {n} xs ys = dfold (λ l → Vec a (l + n)) (λ _ x xs → x ∷ xs) ys xs

_ : dappend (1 ∷ 2 ∷ []) (3 ∷ 4 ∷ []) ≡ 1 ∷ 2 ∷ 3 ∷ 4 ∷ []
_ = refl

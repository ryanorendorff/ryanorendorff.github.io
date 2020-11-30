open import Data.Nat
open import Data.Vec
open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

module DependentFold where

id : {A : Set} → A → A
id x = x

dfold : ∀ {a : Set}
      → {k : ℕ}
      → (p : ℕ → Set)
      → ((l : ℕ) → a → p l → p (suc l))
      → p 0
      → Vec a k
      → p k
dfold {k = 0} p step base [] = base
dfold {k = suc n} p step base (x ∷ xs) = step n x (dfold p step base xs)

dmap : {a b : Set} → {n : ℕ} → (a → b) → Vec a n → Vec b n
dmap {a} {b} {n} f xs = dfold (λ l → Vec b l) (λ _ x xs → f x ∷ xs) [] xs

_ : dmap (λ x → x + 3) (1 ∷ 2 ∷ 3 ∷ []) ≡ (4 ∷ 5 ∷ 6 ∷ [])
_ = refl

dappend : {a : Set} → {m n : ℕ} → Vec a m → Vec a n → Vec a (m + n)
dappend {a} {m} {n} xs ys = dfold (λ l → Vec a (l + n)) (λ _ x xs → x ∷ xs) ys xs

_ : dappend (1 ∷ 2 ∷ []) (3 ∷ 4 ∷ []) ≡ 1 ∷ 2 ∷ 3 ∷ 4 ∷ []
_ = refl

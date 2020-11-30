---
title: Folds that change type at each step!
---

 <!--

This literate file is run through `run.sh`, which imports the clash
libraries. Note that while the clash libraries exist, the regular GHC
compiler is used; no circuits are generated from this example.


> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE DataKinds              #-}
> {-# LANGUAGE KindSignatures         #-}
> {-# LANGUAGE TypeFamilies           #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE NoImplicitPrelude      #-}
> {-# LANGUAGE UndecidableInstances   #-}
>
> -- Allows us to infer additional constraints from the definition
> -- Instead of (KnownNat n, KnownNat (n+2)) =>
> -- we only need (KnownNat n) =>
> {-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
>
> -- Allows us to equate terms like (a + b) + c and a + (b + c) as the same thing.
> {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
>
> module DependentFold where
>
> import Clash.Prelude
> import Data.Singletons.Prelude (TyFun,Apply,type (@@))
> import Data.Proxy
> import qualified Data.List

-->

Outline
=======

- Introduce the basic fold on lists, show how it relates to map
- Introduce vectors (very basic)
- Show vetor `foldr`
- Show how using `foldr` to define `map` does not work
- Introduce dfold
- Define map using dfold
- [Need to do]: find compelling example of using dfold (prefix sum?)
- Compare to agda definition

Notes
=====

In the original paper, the symbol ↠[^1] is used to denote `TyFun`

[^1]: This is to represent `Nat ↠ *`

> something :: Vec 3 Int
> something = 1 :> 2 :> 3 :> Nil

> -- This is exactly the clash dfold, just to make sure we have everything
> --  we need to define the function.
>
> -- Promoting Functions to Type Families in Haskell, R. Eisenberg
> -- Here,we must be careful, though: all types that contain values must
> -- be of kind?in GHC. Thus, GHC requires that the kind of a datatype end
> -- in ... → *, as datatypes are normally meant to hold values. We can now
> -- figure out how ↠ must be defined:

The definition is what you would expect: a fold. It is obscured by the
 requirement of the `Proxy` and `Singleton` components, which are required
 to perform a function at the type level.

> dfold' :: forall p k a . KnownNat k
>        => Proxy (p :: TyFun Nat * -> *) -- Also written as p :: ℕ ↠ Set
>        -> (forall l . SNat l -> a -> (p @@ l) -> (p @@ (l + 1)))
>        -> (p @@ 0)
>        -> Vec k a
>        -> (p @@ k)
> dfold' _ f z xs = go (snatProxy (asNatProxy xs)) xs
>   where
>     go :: SNat n -> Vec n a -> (p @@ n)
>     go _ Nil                        = z
>     go s (y `Cons` (ys :: Vec z a)) =
>       let s' = s `subSNat` d1
>       in  f s' y (go s' ys)


> data Append (m :: Nat) (a :: *) (f :: TyFun Nat *) :: *
> type instance Apply (Append m a) l = Vec (l + m) a
>
> data MapMotive (a :: *) (f :: TyFun Nat *) :: *
> type instance Apply (MapMotive a) l = Vec l a
>
> append' :: (KnownNat m, KnownNat n) => Vec m a -> Vec n a -> Vec (m + n) a
> append' xs ys = dfold' (Proxy :: Proxy (Append m b)) (const (:>)) ys xs
>
> -- This will not work because we need something to provide us with the
> -- next element in the fold
> -- map'' :: (KnownNat m) => (a -> b) -> Vec m a -> Vec m b
> -- map'' f xs = foldr (\a r -> (f a) :> r) Nil xs

> mapList :: (a -> b) -> [a] -> [b]
> mapList f xs = Data.List.foldr (\a r -> f a : r) [] xs
>
> map' :: (KnownNat m) => (a -> b) -> Vec m a -> Vec m b
> map' f xs = dfold (Proxy :: Proxy (MapMotive b)) (const (\a r -> (f a) :> r)) Nil xs
>
> main :: IO ()
> main = do
>   print (map' (\x -> x + 3) something)
>   print (Nil :: Vec 0 Int)
>   print (append' something something)
>   print (mapList (+ 1) [1, 2, 3])

We can define the dependently typed fold in Agda as well as a comparison.
This demonstrates that the type level functions are much easier to encode,
as they are first class constructs in Agda.

```agda
dfold : {a : Set}
      → {k : ℕ} -- Size of input vector
      → (p : ℕ → Set) -- Motive
      → ((l : ℕ) → a → p l → p (suc l)) -- f
      → p 0 -- base
      → Vec a k -- thing to process
      → p k
dfold {k = 0} p step base [] = base
dfold {k = suc n} p step base (x ∷ xs) = step n x (dfold p step base xs)

dmap : {a b : Set} → {n : ℕ} → (a → b) → Vec a n → Vec b n
dmap {a} {b} {n} f xs = dfold (λ l → Vec b l) (λ _ x xs → f x ∷ xs) [] xs

_ : dmap (λ x → x + 3) (1 ∷ 2 ∷ 3 ∷ []) ≡ (4 ∷ 5 ∷ 6 ∷ [])
_ = refl
```

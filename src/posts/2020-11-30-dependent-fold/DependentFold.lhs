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
> import Clash.Prelude hiding (foldr, sum, map)
> import qualified Clash.Prelude as C
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
- Compare to agda definition

Some steps were removed because the post would be too long:

- find compelling example of using dfold (prefix sum?)


Post
====

In functional programming languages like Haskell, we are used to using
higher order functions as a way to simplify control flow. One of the first
higher order functions taught is the fold.

> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr f z []     = z
> foldr f z (x:xs) = f x (foldr f z xs)

The common example for a fold is summing a list.

> sum :: (Num a) => [a] -> a
> sum = foldr (+) 0

For functions like `sum`, at each step we are taking our accumulator value
and combining that with the head of the list, producing something that is
the same shape as the accumulator. Explicitly for a sum on a list of
`Int`s, we can get from one step to the next using this equation

TODO: Add better figure/TeX here describing the type of each piece.

$$
accum_{i + i} = x_i + accum_i
$$

and at step $i + 1$, we will have something with the same shape as what we
had at step $i$ (a single `Int`). No matter how long the input list is, the
result will always be an `Int` that can be stored in the same number of
bytes.

Folds have another trick up their sleeves though; they can be used to _build
up_ a structure, meaning that the shape of the result _changes every step_.
Take the `map` function, which we can define in terms of a fold.

> map :: (a -> b) -> [a] -> [b]
> map f = foldr (\head accum -> f head : accum) []

Here, our equation for how to get from one step to the next is.

TODO: Add better figure/TeX here describing the type of each piece.

$$
accum_{i + 1} = f(x) : accum_i
$$

But now each step takes a list of length `n` and makes a list of length `n + 1`!
This means that the length of the list that results from this type of
fold _depends_ on what list is passed to the function.


Types that depend on values are called dependent types
======================================================

Dependent types is a type theory that allows us to have the type of some
value be predicated by the _value_ of another object. Dependent types allow
us to define a list like type called `Vec` (short for vector) that allows
us to keep track of the length of the list at the type level. The common
definition for a `Vec` is as follows[^1].

< data Vec (n :: Nat) a where
<     Nil :: Vec 0 a
<     Cons :: a -> Vec n a -> Vec (n + 1) a


We use GADTs along with kind signatures to specify that our `Vec` type is
parameterized over a type `a` and a natural number `n`. This is precisely
the same definition as a list, but while constructing the `Vec` keep track
of how many elements we have stored thus far. This is to say that we have
_stored the shape of the value in its type_.

Given that we essentially have a list (with some type level magic), it seems
reasonable that we can define a version of fold for `Vec`.

> vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
> vfoldr f z Nil       = z
> vfoldr f z (x `Cons` xs) = x `f` vfoldr f z xs

This is precisely the same definition as before; the only component that has
changed is that we are keeping track of the length of the input `Vec`. This
means we can define the sum of a vector in the same way as before.

> vsum :: Num a => Vec n a -> a
> vsum = vfoldr (+) 0

So we should be able to recreate map, right?

< vmap :: (a -> b) -> Vec n a -> Vec n b
< vmap f = vfoldr (\head accum -> f head `Cons` accum) Nil
< {-
<   • Couldn't match type ‘n’ with ‘0’
<     ‘n’ is a rigid type variable bound by
<       the type signature for:
<         vmap :: forall a b (n :: Nat). (a -> b) -> Vec n a -> Vec n b
<     Expected type: Vec n a -> Vec n b
<       Actual type: Vec n a -> Vec 0 b
<   • In the expression:
<       vfoldr (\ head accum -> f head `Cons` accum) Nil
<     In an equation for ‘vmap’:
<         vmap f = vfoldr (\ head accum -> f head `Cons` accum) Nil
< -}

Oh no, what happened?! If we look at the type for `vfold`, we will note that
there is a relation between our base value (`z`) and our result type.

< --                      These must be the same b
< --                         ↓               ↓
< vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
<
< vmap f xs = vfoldr (\head accum -> f head `Cons` accum) Nil xs

When Haskell attempts to determine the type for `b`, it gets two equations
(note `~` is type level equality):

- `b ~ Vec n a` (from the result type of `vfold`)
- `b ~ Vec 0 a` (from the type of `Nil`)

Haskell then tries to prove that `Vec n a ~ Vec 0 a` by proving each
component of the type is equal, but this process fails since `n ~ 0`. We
did not run into this case because we had the following equations are equivalent.

- `b ~ a` (from the result type of `vfold`)
- `b ~ a` (from the type of `0`)

In a dependent type context, we can see that the fold that preserves a shape
(`sum`) is fundamentally _different_ from a fold that can change the shape
of its accumulator at each step. A fold that has this ability is sometimes
called a _dependently typed fold_, as the type of each step _depends_ on
the step before.


Defining a dependently typed fold
=================================

Ideally what we would like to do is define a fold where we can specify how
the type changes over each step of the fold.

> -- Copied from Clash.Sized.Vector
> dfold' :: forall p k a . KnownNat k
>        => Proxy (p :: TyFun Nat * -> *) -- Also written as p :: ℕ ↠ Set, called the motive.
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

`TyFun` can really be thought of as `Nat ↠ Type`. Here is one.

> data MapMotive (a :: *) (f :: TyFun Nat *) :: *
> type instance Apply (MapMotive a) l = Vec l a

We can now define the map function by specifying how to move from one step
to the next during the fold.

> vmap :: (KnownNat m) => (a -> b) -> Vec m a -> Vec m b
> vmap f xs = dfold (Proxy :: Proxy (MapMotive b))
>             (const (\a r -> (f a) :> r)) Nil xs

Where does the name "motive" come from? Well this is because `dfold` is
actually the induction principle for vectors!


Comparison to a fully dependently typed language
================================================

We can define the dependently typed fold in Agda as well as a comparison.
This demonstrates that the type level functions are much easier to encode,
as they are first class constructs in Agda.

```agda
-- Nat is named `ℕ` in Agda.
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

-- A proof that dmap does the right thing.
_ : dmap (λ x → x + 3) (1 ∷ 2 ∷ 3 ∷ []) ≡ (4 ∷ 5 ∷ 6 ∷ [])
_ = refl
```

See how much easier that is? :-D

  <!-- Footnotes -->

[^1]: More info on GADT syntax can be found here: TODO

 <!--

Notes
=====

In the original paper, the symbol ↠[^3] is used to denote `TyFun`

[^3]: This is to represent `Nat ↠ *`

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


>
>
> data Append (m :: Nat) (a :: *) (f :: TyFun Nat *) :: *
> type instance Apply (Append m a) l = Vec (l + m) a
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
> something :: Vec 3 Int
> something = 1 :> 2 :> 3 :> Nil
>
> main :: IO ()
> main = do
>   print (sum [1, 2, 3])
>   print (vmap (\x -> x + 3) something)
>   print (Nil :: Vec 0 Int)
>   print (append' something something)
>   print (mapList (+ 1) [1, 2, 3])

<   sum [1, 2, 3]
< ≡ foldr (+) 0 [1, 2, 3]
< ≡ 1 + (foldr (+) 0 [2, 3])
< ≡ 1 + (2 + (foldr (+) 0 [3]))
< ≡ 1 + (2 + (3 + (foldr (+) 0 [])))
< ≡ 1 + (2 + (3 + (0)))
< ≡ 1 + (2 + 3)
< ≡ 1 + 5
< ≡ 6

-->

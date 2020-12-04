---
title: Dependently typed folds
subtitle: folds that change type at each step!
header-includes: |
  \usepackage{fontspec}
  \setmonofont{Iosevka}
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
> import Clash.Prelude hiding (foldr, sum, map, dfold)
> import qualified Clash.Prelude as C
> import Data.Singletons.Prelude (TyFun,Apply,type (@@))
> import Data.Kind
> import Data.Proxy
> import qualified Data.List

-->

In functional programming languages like Haskell, we like to use igher order
functions as a way to simplify control flow. One of the most ubiquitous
higher order functions taught is the fold.

> -- A right fold. If we unroll the fold, it would look like
> -- step x₀ (step x₁ (step x₂ (…)))
> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr step base []     = base
> foldr step base (x:xs) = step x (foldr step base xs)

The fold allows us to consume some data structure (a list) one element at a
time, producing an intermediate result each time an element of the
structure is consumed using the `step` function. For example, we can
convert a list into the sum of its elements by using a `step` function that
adds the intermediate result to each element of the list as it is consumed.

> sum :: (Num a) => [a] -> a
> sum = foldr (+) 0

For folds like `sum`, at each step we are taking our intermediate value and
combining that with an element of the list. When GHC type checks our `sum`
function, it attempts to unify all type variables, meaning that the type
checker looks for types that make sense every place that a type variable is.
For example, if we attempt to figure out `a` and `b` when type checking the
`sum` function

< --             These must be the same b
< --             ↓    ↓     ↓           ↓
< foldr :: (a -> b -> b) -> b -> [a] -> b
<
< sum xs = foldr (+) 0 xs

we can determine what constraints GHC will find for each type variable.
Let's look at the constraints for `b`, using the notation `b ~ constraint`
to say mean "`b` must satisfy the `constraint`".

- `b ~ Num a => a`  (from the base case 0)
- `b ~ a` (from `(+)`)

From these constraints, the compiler will determine that `b` must be an `a`,
as it is the only type that allows all the equations above to be true.

Folds have another trick up their sleeve though; they can be used to _build
up a new structure_. We can define another common higher order function, the
`map` function, in terms of `foldr` to demonstrate this property.

> map :: (a -> b) -> [a] -> [b]
> map f xs = foldr (\value intermediate -> f value : intermediate) [] xs
> --                                       └────────────────────┘
> --                                    The new intermediate value after
> --                                       each step of the fold.

If we do the same analysis for `map` as we did with `sum` to find out what
the type variables should be

< --             These must be the same b
< --             ↓    ↓     ↓           ↓
< foldr :: (a -> b -> b) -> b -> [a] -> b
<
<   map (+ 1) xs
< = foldr (\value intermediate -> value + 1 : intermediate) [] xs
< --                              └──────────────────────┘
< --                           The new intermediate _list_ after
< --                               each step of the fold.

we get the following constraints, where `c` is used to keep track of the
inner type of the list output from `map`.

- `b ~ [c]`  (from the base case `[]`)
- `c ~ a` (by unifying `a` from the input and the `(+ 1)` function)

From these equations, the Haskell compiler can derive an extra constraint
that is noteworthy.

< (a -> b -> b) ~ c -> [c] -> [c] -- (from the function passed to `foldr`)

This constraint means that at each step of the fold, a list is given as
input and a list is returned as output. What is different about this fold
from the `sum` case is that the underlying _structure_ of the intermediate
value in each step of the fold is different. To elaborate a bit more, if we
take the sum of a list of 64-bit `Int`s, our intermediate values after each
invocation of `step` have the same structure.

< --  64 bit Int   64 bit Int
< --       ↓         ↓
< Num a => c -> c -> c
< --            ↑
< --        64 bit Int

If we apply `map` to a list of 64-bit `Int`s, we have a structure that
_grows_ in length each time `step` is called.

< --      Int         [Int]
< --       ∣     of length n + 1!
< --       ↓            ↓
< Num a => c -> [c] -> [c]
< --             ↑
< --           [Int]
< --        of length n

For types that do not keep track of structure (the length of the list),
there is no difference between how the `sum` and `map` folds operate. What
if we did start keeping track of the structure in the type though? Can the
`sum` function and the `map` function still be defined in terms of the same
function?


Types that depend on values are called "dependent types"
========================================================

Dependent types allows us to have the type of some value _depend_ on another
value. This allows us to define a type like a list called called `Vec`
(short for vector) where the length of the list is encoded in the type. The
common definition for a `Vec` is as follows[^1].

[^1]: More info on GADT syntax can be found here: TODO

< data Vec (n :: Nat) (a :: Type) where
<     Nil :: Vec 0 a
<     Cons :: a -> Vec n a -> Vec (n + 1) a

What does the `n :: Nat` notation mean? We are saying that the _type_ of the
input to `Vec` has a _kind_ named `Nat`. A kind is essentially the type of a
type. Types that have a value, such as `Int`, often have the kind `Type`,
while the kind `Nat` represents the _types_ 0, 1, 2, etc.

The `Vec` type we have defined can hold values of type `a` and must be lists
of length `n`[^2]. For example, if we have the type `Vec 4 Int`, we know
that any values of this type must have four elements and stores 4 elements,
for a total of 256 bytes of memory used (for 64 bit `Int`s).

[^2]: More accurately we would say that `Vec` is an _indexed type family_
    that is _parameterized_ by the type `a` and _indexed_ by the natural
    number `n`.

Given that we are working with objects that are the same as lists, it seems
reasonable that we can define a version of fold for `Vec`.

> -- foldr for the Vec type
> vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
> vfoldr step base Nil           = base
> vfoldr step base (x `Cons` xs) = step x (vfoldr step base xs)

This is precisely the same definition as before; the only component that has
changed is that we are keeping track of the length of the input `Vec` and
the names of the data constructors (`Cons` instead of `:`, `Nil` instead of
`[]`). We can define the sum of a vector in the same way as we did on lists.

> vsum :: Num a => Vec n a -> a
> vsum = vfoldr (+) 0

So we should be able to recreate `map` using `vfoldr`, right?

< vmap :: (a -> b) -> Vec n a -> Vec n b
< vmap f = vfoldr (\value intermediate -> f value `Cons` intermediate) Nil
< {-
<   • Couldn't match type ‘n’ with ‘0’
<     ‘n’ is a rigid type variable bound by
<       the type signature for:
<         vmap :: forall a b (n :: Nat). (a -> b) -> Vec n a -> Vec n b
<     Expected type: Vec n a -> Vec n b
<       Actual type: Vec n a -> Vec 0 b
<   • In the expression:
<       vfoldr (\ value intermediate -> f value `Cons` intermediate) Nil
< -}

Oh no, what happened?! Let's look at the constraints that GHC is attempting
to resolve in this case.

< --                      These must be the same b
< --                         ↓               ↓
< vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
<
< vmap f xs = vfoldr (\value intermediate -> f value `Cons` intermediate) Nil xs

- `b ~ Vec n c` (from the result type of `vfold`)
- `b ~ Vec 0 c` (from the type of `Nil`)

The constraints tell us that GHC wants to show that `Vec n a ~ Vec 0 a`
since both types of `Vec` must be of type `b`. GHC rightly tells us that
this is not possible, for it would also have to show that `n ~ 0` for all
possible `n`s. And unfortunately there are more natural numbers than just
zero!

There is a second conundrum in the constraints GHC is trying to solve, but
it only reports the first error. Let's look at the constraints related to
the `step` function to find the second problem.

- `a -> b -> b ~ a -> Vec n c -> Vec (n + 1) c` (from the definition of `vfoldr` and `Cons`)
- `b ~ Vec n c` (from the second argument type in the first constraint)
- `b ~ Vec (n + 1) c` (from the result type in the first constraint)

In a dependent type context, the fold that preserves a structure (`vsum`) is
_fundamentally different_ from a fold that can change the structure at each
step (`vmap`). This is to say that in the case of `vsum`, each invocation
takes and returns the same type for the intermediate value, but for the
`vmap` case, the type of the intermediate value changes after each time the
`step` function is called.

Luckily, it is possible to define a fold where we provide the compiler some
information on how the type changes at each step using a _dependently typed
fold_.


Defining the dependently typed fold on vectors
==============================================

To define a dependently typed fold, we need a few pieces first. First what
we need is a _type level function_ that tells us what our type should be at
before and after each call to `step`. We first define a convient name for
how many steps we have taken so far in the fold

> type StepOfFold = Nat -- StepOfFold is a kind

and can then define the _kind_ of the type level function that tells us what
type we should expect at any step in the fold.

< UpdateType :: StepOfFold -> Type

For example, an instance of `UpdateType` could be the following function (in
pseudocode)

< NatToVec :: Type -> UpdateType
< NatToVec a = \n -> Vec n a

which we could use apply to a specific type and length `n` to make a
complete type[^3].

[^3]: "Complete" here means that the type has kind `Type`. All types that
    hold a value during runtime must have the kind `Type`.

< NatToVec Int 3 == Vec 3 Int

Unfortunately Haskell cannot represent type level functions quite that
cleanly. The first restriction is that the kind `UpdateType` has to be
encoded using the `TyFun` construct

> type UpdateType = TyFun Nat Type -> Type

where `TyFun Nat Type` represents the `UpdateType` we had previously defined
and the the `-> Type` piece is required when returning a data type[^4].

[^4]: The reasoning here is the same as the prior footnote; types that
    eventually will hold a value must end in `-> Type`. This is described in
    the paper "Promoting Functions to Type Families in Haskell" by Richard
    Eisenberg.

Haskell's type level functions don't allow us to define `NatToVec
Int`, as partially applied type level functions are not possible.
Therefore, we must store the type we want to use

> data HoldMyType (a :: Type) (f :: TyFun Nat Type) :: Type
> type instance Apply (HoldMyType a) nth_step = Vec nth_step a

`Apply` is a _type family_ instance which allows us to hold onto some types
before we are passed the current step number, enabling us to bypass the
problem of no partial type level functions. `Apply` can be written infix as `@@`; instead of writing

< (NatToVec Int) 3 == Vec 3 Int

as we had though of the function, we will write

< (HoldMyType Int) @@ 3 == Vec 3 Int

to determine the type at a particular step.

With type level functions, we can now define the dependently typed fold. The
overall function signature is

> -- This function is defined in the Clash library as
> -- Clash.Sized.Vector.dfold
> dfold :: forall p k a . KnownNat k
>       => Proxy (p :: UpdateType)
>       -> (forall l . SNat l -> a -> (p @@ l) -> (p @@ (l + 1)))
>       -> (p @@ 0)
>       -> Vec k a
>       -> (p @@ k)

which is a tad much at once, so let's break it down. The first part of the signature is

< dfold :: forall p k a . KnownNat k =>

which specifies the `KnownNat` constraint for the length of our input vector
`k`. This constraint allows us to pass in the type level natural number of
the input vector into other type level functions through the use of
`Proxy`.

The next line says that our `dfold` function will take in a type level
function for how to generate the type at each step of the fold. This function is sometimes called the _motive_.

< Proxy (p :: UpdateType)

The line

< (forall l . SNat l -> a -> (p @@ l) -> (p @@ (l + 1)))

is the value level function that takes some input value (of type `a`), the
accumulator we have thus far (of type `p @@ l`), and generate the next
accumulator (of type `p @@ (l + 1)`). For example, we could have a function
that adds one to the incoming element and then adds it to the head of the
vector that is passed in.

> update :: SNat l -> a
>        -> ((HoldMyType a) @@ l)       -- Vec l a
>        -> ((HoldMyType a) @@ (l + 1)) -- Vec (l + 1) a
> update l x xs = x `Cons` xs

For this function, we do not need to use `l` when defining the function, as
the increase in the step is taken into account in the function declaration.

The last few lines of the `dfold` type are the the same base case and input
as `foldr` and result, but with the base and result type parameterized by
which step of the fold they are related to (`0` for base, `k` for the end
of the fold).

<       -> (p @@ 0) -- Base case (such as `Nil`)
<       -> Vec k a  -- The `Vec` to fold over
<       -> (p @@ k) -- The type after the final step in the fold.

Now that we have the `dfold` function definition, how is it defined?

> dfold _ f z xs = go (snatProxy (asNatProxy xs)) xs
>   where
>     go :: SNat n -> Vec n a -> (p @@ n)
>     go _ Nil                        = z
>     go s (y `Cons` (ys :: Vec z a)) =
>       let s' = s `subSNat` d1
>       in  f s' y (go s' ys)

If we look at just the definition of `go`, we see that it is the same as
`foldr` with the minor addition of passing around a natural number using
`SNat`. This natural number represents the step we are on; as we go
recursively deeper into the fold, we decrement this number until we hit
zero, in which case we return the base case `z` of type `p @@ 0`. The rest
of the `snatProxy`, `asNatProxy`, and `SNat` components are merely around
to shephard around the changing step number through each step of `go`.


Can we dependently map now?
===========================

We now have all the blocks to define a map function for vectors! We will
start out by defining the type level update function, which will be the
same as `HoldMyType` with a more convenient name.

> data MapMotive (a :: *) (f :: TyFun Nat *) :: *
> type instance Apply (MapMotive a) l = Vec l a

We can now define the map function by passing in our type step function and
the normal `map` value level step function.

> vmap :: forall a b m. (KnownNat m) => (a -> b) -> Vec m a -> Vec m b
> vmap f xs = dfold (Proxy :: Proxy (MapMotive b)) step Nil xs
>   where
>     step :: forall step.
>             SNat step
>          -> a
>          -> MapMotive b @@ step
>          -> MapMotive b @@ (step + 1)
>     step l x xs = f x `Cons` xs

The `forall`s are required to allow the `step` function type refer to the
same `a` as the call to `dfold`.

GHC can inter where to apply `MapMotive` if the definition is inlined,
leading to a more compact definition.

> vmap' :: forall a b m. (KnownNat m) => (a -> b) -> Vec m a -> Vec m b
> vmap' f xs = dfold (Proxy :: Proxy (MapMotive b))
>              (\l x xs -> f x `Cons` xs) Nil xs

This fold is an explicit superset of `vfoldr`. We can define
`vfoldr_by_dfold` by returning the same type at each step of the fold

> data FoldMotive (a :: *) (f :: TyFun Nat *) :: *
> type instance Apply (FoldMotive a) l = a

and applying the function passed to the fold to both arguments in the step
function.

> vfoldr_by_dfold :: KnownNat n => (a -> b -> b) -> b -> Vec n a -> b
> vfoldr_by_dfold f z v = dfold (Proxy :: Proxy (FoldMotive b))
>                         (\l x xs -> f x xs) z v

Finally, we show define the sum function using `dfold` by using
`vfoldr_by_dfold`.

> vsum_by_dfold :: (KnownNat n, Num a) => Vec n a -> a
> vsum_by_dfold = vfoldr_by_dfold (+) 0
> -- Ex: vsum_by_dfold (1 :> 2 :> 3 :> Nil) == 0


Comparison to a fully dependently typed language
================================================

One challenge that came up earlier in the post is the obfuscation of type
level functions through `Proxy`, `TyFun`, `Apply`, and other type level
mechanisms to enable type level functions. In fully dependent type
languages such as Agda, there is no distinction between values, types, or
kinds. We can demonstrate this by reimplementing `dfold` in agda. The type
of `dfold` is


```agda
-- Set is the same as Type in Haskell.
-- Agda needs the specific type `a` passed in, and but the
-- curly braces allow us to omit the types if the Agda compiler
-- can figure it out automatically.
dfold : {a : Set}
      → {k : Nat}
      → (p : Nat → Set) -- Motive
      → ((l : Nat) → a → p l → p (1 + l)) -- step function
      → p 0 -- base case
      → Vec a k
      → p k
```

where the motive function `p` can be defined directly without the use of
`Proxy` or `SNat`. The definition of `dfold` really starts to resemble the
standard `foldr`.

```agda
dfold {k = 0} p step base [] = base
dfold {k = suc n} p step base (x ∷ xs) = step n x (dfold p step base xs)
-- suc k == 1 + k

-- List version for comparison
foldr : {a b : Set} -> (a -> b -> b) -> b -> List a -> b
foldr step base []       = base
foldr step base (x ∷ xs) = step x (foldr step base xs)
```

The only difference between the dependently typed fold on vectors `dfold`
and the fold on lists `foldr` is passing around the type level step number
`n` and the motive `p`. The `subSNat` function in the Haskell `dfold` is
replaced by induction on `k`; a `k + 1` is passed into `dfold` and `k` is
passed down to the recursive call.

We can define the map function as well. The first step is to define the
motive function:

```agda
map_motive : Set -> Nat -> Set
map_motive a l = Vec a l
```

which does not need `Apply`, as it can be partially applied in the
definition of map.

```agda
map : {a b : Set} → {n : ℕ} → (a → b) → Vec a n → Vec b n
map {a} {b} {n} f xs = dfold (map_motive b) (λ _ x xs → f x ∷ xs) [] xs
--                                  ↑
--                     map_motive is partially applied,
--                     leading to a Nat → Set function
```

Fully dependent type languages like Agda allow for type level schenanigans
to be defined much more clearly because there is no difference between
defining a type level and value level function. In fact, a function can
often be used at any level of the type hierarchy; the `+` function can be
used for on `Nat` values, on `Nat` types, on `Nat` kinds, and beyond! (Add
pendantic note about universe of Sets)

To get the same functionality in Haskell, we had to use the following
language extensions.

- `GADTs` to define `Vec`.
- `TypeOperators` to be able to use `+` on the type level.
- `DataKinds` to be able to define type level `Nat`.
- `KindSignatures` to be able to write out `n :: Nat` as a kind.
- `TypeFamilies` to be able to define an instance of `Apply`
- `RankNTypes` to force our step function to work over all steps (the
  `forall l.` part of the `dfold` definition).
- `ScopedTypeVariables` to be able to reference the same `a` in our `step`
   function for `vmap`.

Haskell's dependent type features are exciting; they represent a way to use
dependent types in a mainstream language while keeping all of the current
libraries that exist in Hackage. Hopefully in the future, the syntax and
structure around the type level features become as easy to specify as they
are in Agda, Idris, or any other fully dependently typed language.

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
> -- be of kind * in GHC. Thus, GHC requires that the kind of a datatype end
> -- in ... → *, as datatypes are normally meant to hold values. We can now
> -- figure out how ↠ must be defined:

The definition is what you would expect: a fold. It is obscured by the
 requirement of the `Proxy` and `Singleton` components, which are required
 to perform a function at the type level.

> data Append (m :: Nat) (a :: *) (f :: TyFun Nat *) :: *
> type instance Apply (Append m a) l = Vec (l + m) a
>
> append' :: (KnownNat m, KnownNat n) => Vec m a -> Vec n a -> Vec (m + n) a
> append' xs ys = dfold (Proxy :: Proxy (Append m b)) (const (:>)) ys xs
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

> -- type family Sum (ns :: [Nat]) where
> --   Sum '[] = 0
> --   Sum (n ': ns) = n + Sum ns

>
>
> main :: IO ()
> main = do
>   print $ sum [1, 2, 3]
>   print $ vmap (\x -> x + 3) something
>   print $ (Nil :: Vec 0 Int)
>   print $ append' something something
>   print $ mapList (+ 1) [1, 2, 3]
>   print $ vsum_by_dfold (1 :> 2 :> 3 :> Nil)

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

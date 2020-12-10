---
title: Dependently typed folds
subtitle: folds that change type at each step!
header-includes: |
  \makeatletter
  \def\verbatim@nolig@list{}
  \makeatother

  \usepackage{fontspec}
  \usepackage{xunicode}
  \usepackage{xltxtra} 
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
> import Clash.Prelude hiding (foldr, sum, map, dfoldr)
> import qualified Clash.Prelude as C
> import Data.Singletons.Prelude (TyFun,Apply,type (@@))
> import Data.Kind
> import Data.Proxy
> import qualified Data.List
> import Control.Exception

-->

In functional programming languages like Haskell, we like to use higher order
functions as a way to simplify control flow. One of the most ubiquitous
higher order functions is the fold.

> -- A right fold. If we unroll the fold, it would look like
> -- step x₀ (step x₁ (step x₂ … (step xₙ base)))
> foldr :: (a -> b -> b) -> b -> [a] -> b
> foldr step base []     = base
> foldr step base (x:xs) = step x (foldr step base xs)

The fold allows us to consume some data structure (a list in this case) one
element at a time, producing an intermediate result each time an element of
the structure is passed to the `step` function. For example, we can convert
a list into the sum of its elements by using the `step` function `(+)` that
adds the sum of the elements it has already seen to the next element of the
list to be consumed.

> sum :: (Num c) => [c] -> c
> sum xs = foldr (\value intermediate -> value + intermediate) 0 xs
> --                                     └──────────────────┘
> --                               The new intermediate value after
> --                                    each step of the fold.

To get a better handle on what happens at each step, let's look at the type
that GHC will infer given our type signature. When GHC type checks our `sum`
function, it attempts to unify all type variables, meaning that the type
checker looks for types that make sense every place that a type variable is
used.

< --                    These must be the same type b
< --                      ↓    ↓     ↓           ↓
< --       foldr :: (a -> b -> b) -> b -> [a] -> b
< sum xs = foldr (\value intermediate -> value + intermediate) 0 xs
< --                          ↑          └────────↑─────────┘  ↑ 
< --                     These must be values of the same concrete type

If we want to figure out `a` and `b` when type checking the `sum` function,
we can determine what constraints GHC will use to find the type for each
type variable. Focusing on the constraints for `b`, using the notation `b ~
constraint` to mean "`b` must satisfy the `constraint`"[^1], we can derive
the following.

[^1]: GHC will probably find slightly different constraints to solve; these
    equations were chosen for pedagogical purposes.

- `b ~ Num c => c`  (from the base case 0)
- `b ~ c` (from `(+)`)

From these constraints, the compiler will determine that `b` must be a `c`,
as `c` is the only type that satisfies both constraints above.

Folds have another trick up their sleeve; they can be used to _build up a
new structure_. We can define another common higher order function, the
`map` function, in terms of `foldr` to demonstrate this property.

> map :: (c -> d) -> [c] -> [d]
> map f xs = foldr (\value intermediate -> f value : intermediate) [] xs
> --                                       └────────────────────┘
> --                                  The new intermediate _list_ after
> --                                       each step of the fold.

If we do the same analysis for `map` as we did with `sum` to find out what
the type variables should be

< --                            These must be the same b
< --                            ↓    ↓     ↓           ↓
< --             foldr :: (a -> b -> b) -> b -> [a] -> b
< map (+ 1) xs = foldr (\value intermediate -> value + 1 : intermediate) [] xs
< --                                ↑          └──────────↑───────────┘  ↑ 
< --                            These must be values of the same concrete type

we get the following constraints, where `c` and `d` come from the map function.

- `b ~ [d]`  (from the base case `[]`)
- `d ~ c` (by unifying `c` from the input list and the `(+ 1)` function)

From these constraints, the Haskell compiler can derive an extra constraint
that is noteworthy.

< (a -> b -> b) ~ c -> [c] -> [c] -- (from the step function passed to `foldr`)

This constraint means that at each step of the fold, a list is given as
input and a list is returned as output. What is different about this fold
from the `sum` case is that the underlying _structure_ of the intermediate
value in each step of the fold is different. To elaborate a bit more, if we
take the sum of a list of 64-bit `Int`s, our intermediate values after each
invocation of `step` have the same structure.

< --  64 bit Int   64 bit Int
< --       ↓         ↓
< Num c => c -> c -> c
< --            ↑
< --        64 bit Int

If we apply `map` to a list of 64-bit `Int`s, we have a structure that
_grows_ in length each time `step` is called.

< --     Int         [Int]
< --      ∣     of length n + 1!
< --      ↓            ↓
< step :: c -> [c] -> [c]
< --            ↑
< --          [Int]
< --       of length n

For types that do not keep track of structure (the length of the list),
there is no difference between how the `sum` and `map` folds operate. What
if we did start keeping track of the structure in the type? Can the `sum`
function and the `map` function still be defined in terms of the same fold
function?


Dependent types enable types that keep track of structure
=========================================================

Dependent types allows us to have the type of some value _depend_ on another
value. This allows us to define a type similar to a list called `Vec`
(short for vector) where the length of the list is encoded in the type. The
common definition for `Vec` is as follows[^2].

[^2]: More info on GADT syntax can be found here:
    https://typeclasses.com/ghc/gadt-syntax

< data Vec (n :: Nat) (a :: Type) where
<     Nil :: Vec 0 a
<     Cons :: a -> Vec n a -> Vec (n + 1) a

What does the `n :: Nat` notation mean? We are saying that the _type_ of the
input to `Vec` has a _kind_ named `Nat`. A kind is essentially the type of a
type. Types that have a value, such as `Int`, often have the kind `Type`,
while the kind `Nat` represents the _types_ 0, 1, 2, etc.

The `Vec` type[^3] we have defined can have values of type `a` and must be
lists of length `n`. For example, if we have the type `Vec 4 Int`, we know
that any values of this type must have four elements and each element must
be an `Int`, for a total of 256 bytes of memory used (for 64 bit `Int`s).

[^3]: More accurately we would say that `Vec` is an _indexed type family_
    that is _parameterized_ by the type `a` and _indexed_ by the natural
    number `n`.

Given that we are working with objects that are the same as lists, it seems
reasonable that we can define a fold for `Vec`.

> -- foldr for the Vec type
> vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
> vfoldr step base Nil           = base
> vfoldr step base (x `Cons` xs) = step x (vfoldr step base xs)

This is precisely the same definition as before; the only component that has
changed is that we are keeping track of the length of the input `Vec` and
the names of the data constructors (`Cons` instead of `:`, `Nil` instead of
`[]`). We can define the sum of a vector in the same way as we did on lists.

> vsum :: Num c => Vec n c -> c
> vsum xs = vfoldr (+) 0 xs

So we should be able to recreate `map` using `vfoldr`, right?

< vmap :: (c -> d) -> Vec n c -> Vec n d
< vmap f xs = vfoldr (\value intermediate -> f value `Cons` intermediate) Nil xs
< {-
<   • Couldn't match type ‘n’ with ‘0’
<     ‘n’ is a rigid type variable bound by
<       the type signature for:
<         vmap :: forall c d (n :: Nat). (c -> d) -> Vec n c -> Vec n d
<     Expected type: Vec n d
<       Actual type: Vec 0 d
< -}

Oh no, what happened?! Let's look at the constraints that GHC is attempting
to resolve in this case.

< --                            These must be the same b
< --                          ↓    ↓     ↓               ↓
< --          vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
< vmap f xs = vfoldr (\value intermediate -> f value `Cons` intermediate) Nil xs
< --                              ↑          └────────────↑─────────────┘  ↑ 
< --                     These must be values of the same the same concrete type

- `b ~ Vec n d` (from the result type of `vmap`)
- `b ~ Vec 0 d` (from the type of `Nil`)

The constraints tell us that GHC wants to show that `Vec n d ~ Vec 0 d`
since both types of `Vec` must are associated with the same type variable
`b`. GHC rightly tells us that this is not possible, for it would also have
to show that `n ~ 0` for all possible `n`s. And unfortunately there are more
natural numbers than just zero!

There is a second conundrum in the constraints GHC is trying to solve, but
GHC only reports the first error. To find this error, we need to look at the
`step` function types.

- `(a -> b -> b) ~ c -> Vec n d -> Vec (n + 1) d` (from the definition of
  `vfoldr` and `Cons`)
- `b ~ Vec n d` (from the second argument type in the first constraint)
- `b ~ Vec (n + 1) d` (from the result type in the first constraint)

In a dependently typed context, the fold that preserves a structure (`vsum`)
is _fundamentally different_ from a fold that can change the structure at
each step (`vmap`). This is to say that in the case of `vsum`, each
invocation takes and returns the same type for the intermediate value, but
for the `vmap` case, the _type is different for each intermediate value_.
Luckily, it is possible to define a fold where we provide the compiler some
information on how the type changes at each step using a _dependently typed
fold_.


Defining the dependently typed fold on vectors
==============================================

To define a dependently typed fold, we need a few pieces first. First we
need a _type level function_ that tells us what our type should be before
and after each call to `step`. We define a convenient name for how many
steps we have taken so far in the fold

> type StepOfFold = Nat -- StepOfFold is a kind

and then define the _kind_ of the type level function that tells us what
type we should expect at any step in the fold.

< StepFunction :: StepOfFold -> Type

For example, an instance of `StepFunction` could be the following function (in
pseudocode)

< NatToVec :: Type -> StepFunction
< NatToVec a = \n -> Vec n a

to which we could apply a specific type `a` and length `n` to make a complete
type[^4].

[^4]: "Complete" here means that the type has kind `Type`. All types that
    hold a value during runtime must have the kind `Type`.

< NatToVec Int 3 = Vec 3 Int

Unfortunately Haskell cannot represent type level functions quite this
cleanly. The first restriction is that the kind `StepFunction` has to be
encoded using the `TyFun` construct

> type StepFunction = TyFun Nat Type -> Type

where `TyFun Nat Type` represents the `StepFunction` we had previously
defined and the the `-> Type` piece is required when returning a data type
from the type level step function[^5].

[^5]: The reasoning here is the same as the prior note; types that
    eventually hold a value must end in `-> Type`. This is described in the
    paper "Promoting Functions to Type Families in Haskell" by Richard
    Eisenberg.

When we are defining a `StepFunction` function, we often want to keep track
of an auxiliary type that is constant, for example the `a` we end up needing
to define `Vec n a`. We could use `NatToVec` to hold onto the type of the
`Vec` if we partial apply the function as `NatToVec a`. However, Haskell's
type level functions cannot be partially applied[^6]. Therefore, we must
store the type we want to use in another type, and use the `Apply` type
family to define the `StepFunction` function itself.

[^6]: All type level functions must be complete in the sense that all
    arguments have been given to the type level function when it is called.

> --                                   StepFunction
> --                          ┌───────────────────────────┐
> data HoldMyType (a :: Type) (f :: TyFun Nat Type) :: Type
> type instance Apply (HoldMyType a) nth_step = Vec nth_step a

`Apply` is a type family instance which allows us to hold onto some types
before we are passed the current step number, enabling us to bypass the
problem of no partial type level functions. `Apply` can be written infix as
`@@`; instead of writing

< (NatToVec Int) 3 = Vec 3 Int

as we would with a normal value level function, we will write

< (HoldMyType Int) @@ 3 = Vec 3 Int

to determine the type at a particular step.

With type level functions, we can now define the dependently typed fold. The
function signature is as follows.

> -- This function is copied from the Clash library, where it is
> -- defined as Clash.Sized.Vector.dfold
> dfoldr :: forall p n a . KnownNat n
>       => Proxy (p :: StepFunction)
>       -> (forall l . SNat l -> a -> (p @@ l) -> (p @@ (l + 1)))
>       -> (p @@ 0)
>       -> Vec n a
>       -> (p @@ n)

There is a bunch going on in this type signature, so let's break it down by
each argument to `dfoldr`. The first part of the signature is

< dfoldr :: forall p n a . KnownNat n =>

which specifies the `KnownNat` constraint for the length of our input vector
`n`. This constraint allows us to pass in the type level natural number of
the input vector into other type level functions through the use of
`snatProxy` in the definition.

The next line

< Proxy (p :: StepFunction)

says that our `dfoldr` function will take in a type level function for how
to generate the type at each step of the fold. This function is sometimes
called the _motive_.

The next line

< (forall l . SNat l -> a -> (p @@ l) -> (p @@ (l + 1)))

is the value level function that takes some input value (of type `a`), the
intermediate value we have thus far (of type `p @@ l` for step `l`), and
generates the next intermediate value (of type `p @@ (l + 1)` for step `l +
1`). For example, we could have in a function that adds one to the incoming
element and then combine the result with a vector that is being built up as
the intermediate value.

> step :: Num a
>      => SNat l -- The current step number
>      -> a
>      -> (HoldMyType a) @@ l       -- Vec l a
>      -> (HoldMyType a) @@ (l + 1) -- Vec (l + 1) a
> step l x xs = (x + 1) `Cons` xs

For this particular step function, we do not need to use `l` when defining
the function, as the increase in the step is taken into account in the
function's type.

The last few lines of the `dfoldr` type are the same base case, input
vector, and result type as `foldr`, but with the base and result type
parameterized by which step of the fold they are related to (`0` for base,
`n` for the end of the fold).

< -> (p @@ 0) -- Base case, i.e. the first intermediate value (such as `Nil`)
< -> Vec n a  -- The `Vec` to fold over
< -> (p @@ n) -- The type after the final step in the fold.

Now that we have the `dfoldr` function type, how is it defined?

> -- If we unroll the dependent fold, it would look like
> -- step n x₀ (step (n - 1) x₁ (step (n - 2) x₂ … (step 0 xₙ base)))
> dfoldr _ step base xs = go (snatProxy (asNatProxy xs)) xs
>   where
>     go :: SNat l -> Vec l a -> (p @@ l)
>     go _         Nil                       = base
>     go step_num (y `Cons` (ys :: Vec z a)) = 
>       let step_num_minus_one = step_num `subSNat` d1 -- (d1 is the same as 1)
>       in  step step_num_minus_one y (go step_num_minus_one ys)

If we look at just the definition of `go`, we see that it is the same as
`foldr` with the minor addition of passing around a natural number using
`SNat`. This natural number represents the step we are on; as we go
recursively deeper into the fold, we decrement this number until we hit
zero, in which case we return the base case `base` of type `p @@ 0`. The
rest of the `snatProxy`, `asNatProxy`, and `SNat` components are to shepherd
around the changing step number through each call to `go`.

Notice that the motive function `p` is not used in the definition of
`dfoldr`. This is because the function is used only at the type level; a
value level `Proxy` object is used to pass around the motive function to
other functions and has no tangible use as a value.


Can we dependently map now?
===========================

We now have all the blocks to define a map function for vectors! We will
start out by defining the type level step function, which will be the
same as `HoldMyType` with a more descriptive name.

> data MapMotive (a :: Type) (f :: TyFun Nat Type) :: Type
> type instance Apply (MapMotive a) l = Vec l a

We can now define the map function by passing in our type step function and
the normal `map` value level step function. The `forall`s are required to
allow the `step` type signature to refer to the same `c` and `d` that appear
in the signature for `vmap`[^7].

> vmap :: forall c d n. (KnownNat n) => (c -> d) -> Vec n c -> Vec n d
> vmap f xs = dfoldr (Proxy :: Proxy (MapMotive d)) step Nil xs
>   where
>     step :: forall step.
>             SNat step
>          -> c
>          -> MapMotive d @@ step
>          -> MapMotive d @@ (step + 1)
>     step l x xs = f x `Cons` xs

[^7]: Haskell is a bit strange with the type level function. If `MapMotive`
    is passed anything specified in the `forall` that is not `d`, it will
    correctly comment that argument passed to `MapMotive` does not unify
    with the other types. However, it will happily accept any other
    unspecified type variable. My assumption here is that if a new variable
    is introduced (say `δ`), it will implicitly add the `forall
    δ_new_identifier` qualification with a new identifier during type
    inference (as to not alias with an existing α) and then conclude
    `δ_new_identifier ~ d` as the only way to make the types unify.
    Dependent languages similar to Haskell like Agda will not automatically
    universally quantify a stray type variable for you as generalized type
    inference is undecidable in a dependently typed context (see
    https://cs.stackexchange.com/a/12957).

GHC can infer where to apply the `MapMotive` function if the definition of
the step is inlined, leading to a more compact definition.

> vmap' :: forall c d n. (KnownNat n) => (c -> d) -> Vec n c -> Vec n d
> vmap' f xs = dfoldr
>   (Proxy :: Proxy (MapMotive d))                         -- type level step
>   (\l value intermediate -> f value `Cons` intermediate) -- value level step
>   Nil xs

So we now have a way to specify `vmap` that GHC is happy with! Let's break
down the constraints GHC reconstructs to be able to verify why this works
for ourselves. The first problem we had was that the base case passed in had
a different type than the result (`Vec 0 a ~ Vec n a`).

- `p @@ 0 ~ (MapMotive d) @@ 0 ~ Vec 0 d`
- `p @@ n ~ (MapMotive d) @@ n ~ Vec n d`

This problem does not occur for `vmap` because we no longer need to unify
the base and end case; they are distinct types in the dependent fold. What
about the second problem, where the step function changes the type? For that
conundrum,

- `p @@ l ~ (MapMotive d) @@ l ~ Vec l d`
- `p @@ (l + 1) ~ (MapMotive d) @@ (l + 1) ~ Vec (l + 1) d`
- `SNat l -> a -> p @@ l -> p @@ (l + 1) ~ SNat l -> c -> Vec l d -> Vec (l + 1) d`
  (from unifying the step function for `dfoldr` and the step function
  defined in `vmap`)
  
we can see that the last constraint is consistent between the definition of
`dfoldr` and the definition of `vmap`.


One fold to rule them all
=========================

The `dfoldr` fold is a strict superset of `vfoldr`. To demonstrate this, we
can define `vfoldr_by_dfoldr` by returning _the same type_ at each step of
the fold

> data FoldMotive (a :: Type) (f :: TyFun Nat Type) :: Type
> type instance Apply (FoldMotive a) l = a

and applying the `step` function to the fold to both the incoming and
intermediate value.

> vfoldr_by_dfoldr :: KnownNat n => (a -> b -> b) -> b -> Vec n a -> b
> vfoldr_by_dfoldr step base xs =
>   dfoldr (Proxy :: Proxy (FoldMotive b))
>         (\l value intermediate -> step value intermediate) base xs

Finally, we define the sum function using `dfoldr` via `vfoldr_by_dfoldr`.


> vsum_by_dfoldr :: (KnownNat n, Num c) => Vec n c -> c
> vsum_by_dfoldr = vfoldr_by_dfoldr (+) 0
> -- Ex: vsum_by_dfoldr (1 :> 2 :> 3 :> Nil) == 6

Since dependently typed folds subsume the standard folds[^8], it would be
possible to replace all folds with their dependent counterpart. I have
doubts that this would be a good direction for Haskell; while we can likely
automatically derive the definition of the standard fold from the dependent
fold, the developer implementing an instance of the `Foldable` typeclass
would now need to use dependent types, which may be a rough barrier
especially if the developer has no intention for the fold to be used in a
dependent context.

[^8]: There is a version of dependent fold for lists as well, which can be demonstrated in Agda as follows.
```agda
dfoldr : {a : Set} {p : List a → Set}
       → ((x : a) → {xs : List a} → p xs → p (x ∷ xs))
       → p []
       → (l : List a)
       → p l
dfoldr step base [] = base
dfoldr step base (x ∷ xs) = step x (dfoldr step base xs)
```
The Agda variant uses a value level list reflected into the type (both for `l` and `xs`), which is hard/potentially not possible to do in Haskell at the moment.

 <!--
 
What is neat about all these dependent folds is that they codify the notion
of proof by induction; you can use dependently typed folds to prove
properties about a program! For example, you could use `dfoldr` to show that
the `vmap` function, when passed `id`, returns the same list. That is
something that is much easier to demonstrate in Agda though. I may try to
demonstrate the proof in Haskell and make that a different post.

-->


Comparison to a fully dependently typed language
================================================

One challenge that came up earlier in the post was the obfuscation of type
level functions through `Proxy`, `TyFun`, `Apply`. In fully dependent type
languages such as Agda, there is no distinction between values, types, or
kinds. This means that when we define a function, we can use it at _any
level_ in the type hierarchy. We can demonstrate this by reimplementing
`dfoldr` in Agda. The type of `dfoldr` in Agda is


```agda
-- Set is the same as Type in Haskell.
-- Agda needs the specific type `a` passed in; the curly
-- braces allow us to omit the types if the Agda compiler
-- can figure the types out automatically.
dfoldr : {a : Set}
       → {n : Nat}
       → (p : Nat → Set) -- Motive
       → ((l : Nat) → a → p l → p (1 + l)) -- step function
       → p 0 -- base case
       → Vec a n
       → p n
```

where the motive function `p` can be defined directly without the use of
`Proxy`, and applied without a special operator (`@@`). The definition of
`dfoldr` really demonstrates just how similar the regular and dependently
typed fold are.

```agda
dfoldr {n = 0}       p step base []       = base
dfoldr {n = suc n-1} p step base (x ∷ xs) = step n-1 x (dfold p step base xs)
-- suc n == 1 + n

-- List version for comparison; ∷ and [] can be used for either
-- List or Vec, where the type is inferred by context.
foldr : {a b : Set} -> (a -> b -> b) -> b -> List a -> b
foldr step base []       = base
foldr step base (x ∷ xs) = step x (foldr step base xs)
```

The only difference between the dependently typed fold on vectors and the
fold on lists is passing around the type level step number `n` and the
motive `p`. The `subSNat` function in the Haskell `dfoldr` is replaced by
induction on `n`; a `n + 1` is passed into `dfoldr` and `n` is passed down to
the recursive call.

We can define the map function as well. The first step is to define the
motive function

```agda
map_motive : Set -> Nat -> Set
map_motive a l = Vec a l
```

and then pass this function into `dfoldr` by _partially applying_ it with the
type of the output vector.

```agda
vmap : {c d : Set} → {n : ℕ} → (c → d) → Vec c n → Vec d n
vmap {c} {d} {n} f xs = dfoldr (map_motive d) (λ _ x xs → f x ∷ xs) [] xs
--                                   ↑
--                      map_motive is partially applied,
--                      leading to a Nat → Set function
```

Fully dependent type languages like Agda allow for type level shenanigans to
be defined much more clearly because there is no difference between defining
a type level and value level function. In fact, a function can often be used
at any level of the type hierarchy; the `+` function can be used on `Nat`
values, on `Nat` types, on `Nat` kinds, and beyond[^9]!

[^9]: In Haskell, the hierarchy of objects is `values -> types -> kinds ->
    sorts`. Languages like Agda take this further and define a _universe_
    type hierarchy, which is indexed by a natural number. Specifically, in
    Agda the hierarchy is `Set₀ -> Set₁ -> Set₂ -> …`
    
To get the same functionality in Haskell, we had to go through quite a bit
of extension gymnastics.

- `GADTs` to define `Vec`.
- `TypeOperators` to be able to use `+` on the type level.
- `DataKinds` to be able to define type level `Nat`.
- `KindSignatures` to be able to write out `n :: Nat` as a kind.
- `TypeFamilies` to be able to define an instance of `Apply`
- `RankNTypes` to require that the step function passed to `dfoldr` works
  for all steps (the `forall l.` part of the `dfoldr` definition).
- `ScopedTypeVariables` to be able to reference the same `c` and `d` in our
  `step` function as in the top level `vmap` function.

Haskell's dependent type features are exciting; they represent a way to use
dependent types in a mainstream language while keeping all of the current
libraries that exist in Hackage. The current developer experience of using
dependent types is a tad challenging, but as dependent types become more
fleshed out in Haskell the experience should improve.

 <!--

> testVector :: Vec 3 Int
> testVector = 1 :> 2 :> 3 :> Nil

> test :: Bool -> String -> IO ()
> test b s = if b then return () else (putStrLn s >> return ())

> -- Basic testing here just to make sure the functions are somewhat sanely
> -- implemented
> main :: IO ()
> main = do
>   test (sum [1, 2, 3] == 6)
>     "Definition of `sum` is failing"
>   test (vmap (\x -> x + 3) testVector == 4 :> 5 :> 6 :> Nil)
>     "Definition of `vmap` is failing"
>   test (vsum_by_dfoldr testVector == 6)
>     "Definition of `vsum_by_dfoldr` is failing"
>   print "Run complete"

-->

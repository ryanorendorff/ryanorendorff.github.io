---
title: Using ChatGPT to prove math in Agda
subtitle: or how to spend hours with an imprecise learner
---

<!--

Module code to make the rest of the file compile in Agda

```
open import Agda.Builtin.Nat hiding (_+_)

open import Data.Product
open import Relation.Nullary

open import Relation.Binary.PropositionalEquality
open ≡-Reasoning

module GPTAgda where

ℕ : Set
ℕ = Nat
```

-->

I have been playing around with [ChatGPT](https://openai.com/blog/chatgpt/)
(like everyone else). I attempted to get ChatGPT to prove some basic properties
of mathematics in [Lean](https://leanprover-community.github.io/), but this
turned out quite poorly. ChatGPT is quite happy to attempt to use a template for a
proof it has seen—such as the proof that the square root of 2 is irrational—in a
case where the proof is false for other inputs.  In addition, ChatGPT produced
Lean code that was almost correct but had some bugs in it.

I am far more familiar with Agda, so I was curious if I could get ChatGPT to
generate a proper proof that the Agda compiler would accept. In this exercise, I
managed to get ChatGPT to prove that addition is commutative. But as we will
see, it makes some interesting stumbles on the way.

I've edited some of the responses from ChatGPT to reduce the amount of
explanatory text it likes to spit out.  Even when I tell it to not explain
things to me, it really can't help itself!

My prompts are in quoted italics, while ChatGPT's responses are in code block
sections.


# Now on with the show!

_"In this exercise, we will be proving properties about simple programs using
Agda. I would like you to use unicode characters and you have access to the
standard library for Agda. In addition, please provide only the code related to
the proofs without explanation. Are you ready?"_

~~~agda
-- O_o you ok there ChatGPT?
open import Agda.Primitive

postulate
  Set : Set
  _∈_ : Set → Set → Set
  _⊆_ : Set → Set → Set
  _∪_ : Set → Set → Set
  _∩_ : Set → Set → Set

infixr 40 _∈_
infix 4 _⊆_

_⊂_ : Set → Set → Set
A ⊂ B = A ⊆ B ∧ ¬ (A ∈ B)

transitivity : {A B C : Set} → A ⊂ B → B ⊂ C → A ⊂ C
transitivity A ⊂ B B ⊂ C = B ∈ A ∧ C ∈ B
~~~

I am curious how this popped into the AI's statistical mindset, and also why it
decided to provide this code without prompting. In this case ChatGPT does a few
odd things to say the least:

1. It defines `Set : Set`. If this were true in Agda, then our proof systems
   would enable paradoxes like [Russell's
   paradox](http://liamoc.net/posts/2015-09-10-girards-paradox.html)!  This is
   part of the reason why many theorem proving languages have a [infinite
   hierarchy of
   universes](https://agda.readthedocs.io/en/v2.6.2.2/language/universe-levels.html).
2. It only postulates some operators, but assume others (like `¬` and  `∧`)
   exist.
3. The proofs are just bonkers. The definition of `⊂` and `transivitity` both
   use the inputs like they are sets (`A ⊆ B`) and like they are elements (`A ∈
   B`); I assume it meant `A = B` instead of `∈`.

I assume that at some point ChatGPT saw a jumble of symbols related to set
theory in its training data, along with some classical first order logic
symbols, and just made a unicode symbol soup out of it.  It is impressive that
it knows some relation between the symbols though!

Ok, so with that I decided to retry the prompt to get back on track. I got the
following response.

> Yes, I am ready. Please provide the details of the proof that you would like
  me to provide.

_"Great! Can you define the addition function on natural numbers for me? Please
define the function using an infix symbol."_

<!-- Copied here because code in block quotes is not parsed as literate Agda code.

```agda
_+_ : ℕ → ℕ → ℕ
zero + m = m
(suc n) + m = suc (n + m)
```

-->

> Here is a possible definition of the addition function on natural numbers
> using infix syntax:
>
> ~~~agda
> _+_ : ℕ → ℕ → ℕ
> zero + m = m
> (suc n) + m = suc (n + m)
> ~~~
>
> This function uses pattern matching on the left argument to define the behavior
> of the function for the zero and suc cases. The zero case simply returns the
> right argument, while the suc case recursively applies the function to the
> predecessor of the left argument and the right argument, and wraps the result in
> a suc constructor. This results in the expected behavior for natural number
> addition.

This definition is correct and the explanation is correct, if a bit oddly
worded. This particular definition occurs so widely in documents talking about
addition on natural numbers that it is not particularly surprising that we get
back the correct result.

_"Where can I import the symbol ℕ from in the Agda standard library?"_

> The symbol ℕ is defined in the Agda.Builtin.Nat module of the standard library.
> To import this module and use the ℕ symbol, you can add the following import
> statement at the top of your Agda file:
>
> ~~~agda
> open import Agda.Builtin.Nat
> ~~~
>
> This will make the `ℕ` symbol available for use in your code. Note that you
> can also use the shorter alias `Nat` instead of the full module name
> `Agda.Builtin.Nat` in the import statement.

Note that you _can't_ access the builtin with just `Nat` (or I could not on my
up to date Agda 2.6.2.2 install). So a neat tip, but also not quite a true one.
In addition, it is more likely that someone using the standard library would use
the definition of natural numbers exported from the `Data.Nat` module.

_"open import Agda.Builtin.Nat will import Nat, not ℕ. Can you make an
alias for Nat named ℕ?"_

> Yes, it is possible to create an alias for the `Nat` symbol with the name `ℕ`
> in Agda. Here is an example of how you can do this:
>
> ~~~agda
> open import Agda.Builtin.Nat
>
> ℕ : Set
> ℕ = Nat
> ~~~

This is interesting, as the chat bot has picked up the relation between the two
tokens and how to relate them in Agda.

_"Ack! The Agda.Builtin.Nat also defines a function named \_+\_. Can you hide
\_+\_ while importing Agda.Builtin.Nat?"_

> Yes, it is possible to hide the _+_ function from the Agda.Builtin.Nat module
> when importing it in your Agda code. To do this, you can use the hiding keyword
> in the open import statement, like this:
>
> ~~~agda
> open import Agda.Builtin.Nat hiding (_+_)
> ~~~
>
> This will import all the symbols from the Agda.Builtin.Nat module except for the
> \_+\_ function. This allows you to define your own \_+\_ function without naming
> conflicts with the one from the standard library.
>
> Alternatively, you can also use the renaming keyword in the open import
> statement to import the \_+\_ function from the standard library under a different
> name, like this:
>
> ~~~agda
> open import Agda.Builtin.Nat renaming (_+_ to plus)
> ~~~
>
> This will import the \_+\_ function from the Agda.Builtin.Nat module and rename it
> to plus, which can then be used in your code without conflicts with your own \_+\_
> function.

This is a neat side effect of the chat bot. It provided the correct answer in
this case, and gave me another convenient and related feature in Agda I might
otherwise not know about.

_"Great. Now let's prove that addition is commutative."_

~~~agda
_+_ : ℕ → ℕ → ℕ
zero + m = m
(suc n) + m = suc (n + m)

commutative : ∀ m n → m + n ≡ n + m
commutative zero n = refl
commutative (suc m) n = cong suc (commutative m n)
~~~

This code is not correct but it does use a pattern we will see come up a few
times that I will call the "congruence on inductive hypothesis" pattern[^1].  It
is rather common for simple proofs to be defined such that the base case is
proven by reflexivity (`refl`), while the inductive case is some simple
congruence relation (often using a type's data constructor) on top of the
inductive hypothesis. ChatGPT will attempt to use this strategy a lot, as we
shall see.

[^1]: If someone knows a real name for this I would love to know.

_"This code is not quite correct. Let's break down the errors one at a time._

_The first problem is that the first clause can not be proved by reflectivity
(refl). This clause needs to prove that n ≡ n + 0, but it does not know how to
do this. Can you define a proof for n ≡ n + 0 named +-identity-r and use it
correctly in the first clause of commutative?"_


<!--

```agda
+-identity-r : ∀ n → n ≡ n + zero
+-identity-r zero = refl
+-identity-r (suc n) = cong suc (+-identity-r n)
```

-->

~~~agda
-- Correct! Good job.
+-identity-r : ∀ n → n ≡ n + zero
+-identity-r zero = refl
+-identity-r (suc n) = cong suc (+-identity-r n)

commutative : ∀ m n → m + n ≡ n + m
commutative zero n = +-identity-r n -- Correct use!
commutative (suc m) n = cong suc (commutative m n)
~~~

ChatGPT gets the definition of `+-identity-r` correct (as an application of the
"congruence on inductive hypothesis" pattern) and also applied it in the right
clause. I find it impressive that the bot understood what was meant by a clause
and correctly modified the implementation, including the correct argument.

_"Great work! Now onto the second error._

_In the second clause, we have accidentally proven suc (m + n) ≡ suc (n + m)
instead of the desired suc m + n ≡ n + suc m. To complete this proof, we will
need to show that suc (m + n) ≡ n + suc m._

_Write a function named suc-distr-right that proves suc (m + n) ≡ m + suc n."_

~~~agda
-- It tried to prove a variant of associativity
suc-distr-right : ∀ m n p → p + suc (m + n) ≡ suc (p + m) + n
suc-distr-right zero n p = refl
suc-distr-right (suc m) n p = cong suc (suc-distr-right m n p)

commutative : ∀ m n → m + n ≡ n + m
commutative zero n = +-identity-r n
commutative (suc m) n = suc-distr-right m n n -- Incorrect use here :/
~~~

I'm not sure why ChatGPT went on this associativity tangent.  Additionally it
seems that the bot gets a bit too excited to do the clause replacements when it
defines something new, so it does the replacement of the second clause of
`commutative` when it should have not made any modification yet. 

_"This is unfortunately incorrect. suc-distr-right only takes in two arguments,
and should prove suc (m + n) ≡ m + suc n."_

<!--

```agda
suc-distr-right : ∀ m n → suc (m + n) ≡ m + suc n
suc-distr-right zero n = refl
suc-distr-right (suc m) n = cong suc (suc-distr-right m n)
```

-->

~~~agda
suc-distr-right : ∀ m n → suc (m + n) ≡ m + suc n
suc-distr-right zero n = refl
suc-distr-right (suc m) n = cong suc (suc-distr-right m n)

commutative : ∀ m n → m + n ≡ n + m
commutative zero n = +-identity-r n
commutative (suc m) n = suc-distr-right m n -- Well it got the number 
                                            -- of arguments right.
~~~

_"This is so close! But the second clause of commutative needs to use both
suc-distr-right and the induction hypothesis. Can you fix the second clause of
commutative?"_

~~~agda
commutative : ∀ m n → m + n ≡ n + m
commutative zero n = +-identity-r n
commutative (suc m) n = cong suc (commutative m n) ∘ suc-distr-right m n
~~~

This is an interesting attempt: ChatGPT has correctly identified that the
induction hypothesis is `commutative m n` and does attempt to connect two things
together (using function composition `_∘_`), but this operator connects
_functions_ not _proofs_. To combine proofs we will need something else.

In the meantime I thought I would try to ask ChatGPT where it decided to get the
compose operator from.

_"What is the symbol ∘, and where is it defined?"_

> In Agda, the symbol ∘ is the composition operator, which is used to combine
> two functions or proofs. It is defined in the Agda.Builtin.Function module of
> the standard library.

_"Not quite, the ∘ operator is imported from Function. It is also not the
operator you need to prove the second clause of commutative._

_Can you fix the second clause of commutative? Hint, you need to use the
trans function."_

~~~agda
-- It includes this import but now doesn't need it.
open import Agda.Builtin.Function

commutative : ∀ m n → m + n ≡ n + m
commutative zero n = +-identity-r n
commutative (suc m) n = trans (suc-distr-right m n) (commutative m n)
~~~

This result is funny: the use of `trans` is correct but it flipped the
arguments! It would have been closer to the right answer had it not decided to
flip the arguments. However, since the `_∘_` operator operates right to left and
function calls are left to right, making this flip does make some sense.

Also somehow we lost the use of the "congruence on induction hypothesis" pattern
here, which we actually do require!

_"Almost! There are three problems in the second clause of commutative._

1. _The first and second arguments to trans need to be flipped._
2. _The first and second arguments to suc-distr-right need to be flipped._
3. _The inductive call to commutative needs to use cong, like you had before._

_Can you make these three changes for me?"_

~~~agda
commutative : ∀ m n → m + n ≡ n + m
commutative zero n = +-identity-r n
commutative (suc m) n = trans (commutative m n) (suc-distr-right n m) ∘ cong suc
~~~

Well ChatGPT adds the needed `cong suc` but attempts to tack it on again using
function composition 😆. Also it has correctly flipped the arguments to
`suc-distr-right` for some reason. Maybe ChatGPT saw the error of its ways!

_"So close! The use of cong suc needs to happen before the call to
commutative."_

```agda
commutative : ∀ m n → m + n ≡ n + m
commutative zero n = +-identity-r n
commutative (suc m) n = trans (cong suc (commutative m n)) (suc-distr-right n m)
```

_You got it! Congratulations. Let's celebrate._

> Thank you, I'm glad that I was able to help you with this proof. It was a
> challenging problem, but I enjoyed working on it. I hope you found the
> solution to be satisfactory. Is there anything else you would like to learn
> about or discuss? I am here to help you with any questions you may have.

_Nope, time to party!_

> Great! I'm glad you're happy with the solution. I hope you have a wonderful
> time celebrating. Thank you for the opportunity to help you with this proof.
> Have a great day!


# ChatGPT, your imprecise editing friend

ChatGPT is not versed trained on proofs directly, but rather finds correlations
between tokens/words. What this exercise mostly ended up boiling down to was
telling ChatGPT how to make some text edit and then letting it put strings in
the right place. It often chooses a similar idea to what you want—transitivity
versus composition, or products versus boolean and—but doesn't necessarily
choose the right one. When you can convince it of the right symbols, it
sometimes decides to edit some other part and make the proposed code farther
from the solution. Or it will bring back a symbol because it has seen it before,
even if its original and current use is wrong.

One interesting catch here is that for this exercise to work, I had to be quite
careful with the names of functions. For example, in a different chat thread I
had asked ChatGPT to define proofs like `suc-distr-right` with the names like
`suc[m+n]≡m+suc[n]` (a common trick so I remember what is being proved). But
this ended up being disastrous: a lot of proofs will add tick marks to variable
names like so

~~~agda
suc-distr-right : ∀ m n → suc (m + n) ≡ m + suc n
suc-distr-right zero n = refl
suc-distr-right (suc m') n = cong suc (suc[m+n]≡m+suc[n] m' n)
~~~

and when the variable names are in the function name, ChatGPT would add tick
marks into uses of the function name!

~~~agda
suc-distr-right : ∀ m n → suc (m + n) ≡ m + suc n
suc-distr-right zero n = refl
suc-distr-right (suc m') n = cong suc (suc[m'+n]≡m'+suc[n] m' n)
--                                          ↑     ↑
--                                       NOOOOOOOOOOOO!!! :(
~~~

My first experiment on this blog post failed because no matter what I did, I
could not convince ChatGPT to not use tick marks or to stop using them in
function names.


# Merging formal methods and AI

This is definitely a fun experiment, but there are other AI systems working more
directly on attempting to prove properties. For example, Google's
[Minerva](https://ai.googleblog.com/2022/06/minerva-solving-quantitative-reasoning.html)
is rather good at handling simple word problems (including some from the
International Math Olympiad). Minerva stumbles on occasion though, either
providing an incorrect proof or an incorrect result entirely.

I am excited by some current developments that involve [marrying a proof system
like Lean with an AI proof search](https://arxiv.org/pdf/2205.11491.pdf). Much
of the other work on proving mathematics by AI has been trained with known
input/output pairings from some trusted oracle; adding a proof system into the
mix can allow the AI to find new proofs and _validate that its answer is sound_.
The AI could still potentially fail on translating some prompt into the correct
statement to prove (as shown by the original associative definition of
`suc-distr-right`), but there might be a way to mitigate this as well.

To truly aid the working mathematician, an AI assistant would need to propose
clever creative insights that are not obvious at first but move the proof
forward.  Math often involves finding some interesting mathematical
transformation or framework to reframe the problem statement into. By performing
a transformation into another domain, some difficult proofs can become quite a
bit easier.  If AI systems can shift to making some of these creative leaps then
it will go from a useful tool for reducing proof boilerplate to a true companion
on a mathematician's journey to explore the mathematical landscape.


# Appendix: Proving ChatGPT's set statements

Originally ChatGPT gave us some statements that looked roughly similar to
properties defined on sets. While the results that it produced did not encode
sets or set operations properly, we can do so in Agda.

First we can define a set by its membership function. Note here we are not using
the `Set : Set` definition; to encode our set definition, we will need to say
that our set lives within some higher type universe called `Set₁`.

```agda
-- A subset is defined as a predicate on a set:
-- we can view Subset as the elements of Set that
-- satisfy some property.
Subset : Set → Set₁
Subset A = A → Set
```

In the Agda standard library, this is called
[Pred](https://github.com/agda/agda-stdlib/blob/625a5775f0a2b4e56c9904278294e7e7f735b737/src/Relation/Unary.agda#L38);
we are making our own (equivalent) definition here to better match what ChatGPT
was attempting.

With our definition[^2], we can now define operators such as `∈` and `⊆` on
sets. This is more useful than just postulating that these operators exist since
we can now use the definition of these operators when attempting to prove
properties.

[^2]: The same definition is [defined on Stack
      Overflow](https://stackoverflow.com/questions/61037572/how-to-define-the-range-function-on-a-relation-in-agda-set-theory)
      should you want to see more properties being proved on subsets.

```agda
-- We just extract our inclusion function and see if it applies
-- to the element.
_∈_ : {A : Set} → A → Subset A → Set
a ∈ P = P a

-- A value is not in a set if we can prove having that element
-- in the set would lead to a contradiction.
_∉_ : {A : Set} → A → Subset A → Set
a ∉ p = ¬ (a ∈ p)

-- A subset is just to say that if we find the element in A that
-- then we can show we can find it in B too.
_⊆_ : {A : Set} → Subset A → Subset A → Set
A ⊆ B = ∀ {x} → x ∈ A → x ∈ B

-- Defines priority of operators in infix notation.
infix 4 _∈_ _∉_ _⊆_
```

Now to define a [strict subset](https://en.wikipedia.org/wiki/Subset), we need
to show that for some sets A and B, there exists at least one element of B that
is not in A. ChatGPT attempted this with the statement `A ⊂ B = A ⊆ B ∧ ¬ (A ∈
B)`, but mixed up the symbols that should be used:

1. Since we have defined `Subset` similarly to the way ChatGPT attempted, we
   need to use the product `×` instead of boolean `∧`.
2. ChatGPT was attempting to say that we need to show something in B is not in
   A. To encode this property, we will need to use an
   [existential](https://plfa.github.io/Quantifiers/).


```agda
-- A strict subset is a subset with an
-- additional proof that there exists an x such that
-- x is not in the first set.
_⊂_ : {A : Set} → Subset A → Subset A → Set
A ⊂ B = (A ⊆ B) × ∃[ x ] (x ∈ B × x ∉ A)
--         ↑      └─────────↑──────────┘
--    Original subset       |
--               and the proof that some x in B and is not in A

infix 4 _⊂_
```

Finally we can do our proof of transitivity.

```agda
transitivity : ∀ {S : Set} → {A B C : Subset S}
             → A ⊂ B → B ⊂ C → A ⊂ C
transitivity
  (A⊆B , _)                 -- A ⊂ B broken into its pieces
  (B⊆C , x , x∈C , x∉B)     -- B ⊂ C broken into its pieces
  =
  (λ x∈A → B⊆C (A⊆B x∈A)) , -- A ⊆ C
  (x , x∈C ,                -- There exist an x such that x ∈ C
  (λ x∈A → x∉B (A⊆B x∈A)))  -- and x ∉ A by contradiction
```

ChatGPT was correct to define A, B, and C as implicit arguments but missed the
mark on the definition itself. Let's briefly run through how this works.

1. First we deconstruct our strict subsets so we can access both their internal
   subset relation and the proof that an element exists in the right set but not
   in the left.
2. In our definition, we need to construct a strict subset as an output. We can
   do this in pieces. We first define our regular subset relation by saying that
   for some `x ∈ A`, we can derive that `x ∈ B` using `A⊆B`, and then finally
   derive x ∈ C using `B⊆C` on our value of `x ∈ B`.
3. We now need to prove that some x in C is not in A. To do so, we take the x we
   defined in our `B ⊂ C` relation and apply it in two ways. First we have to
   show that `x ∈ C`, which luckily we have a proof of from our `B ⊂ C`
   argument. Then we need to show that this x we have chosen cannot be in A. 
   
   We will show x is not in A by contradiction (the last line in the code
   block). Say we did have a proof that x was in A (called `x∈A`). Then using
   our relation `A⊆B`, we can show that x must be in B. But from our `B ⊂ C`
   relation, we know that x cannot be in B (from `x∉B`). And so we have a
   contradiction!
   
This proof is not incredibly involved, but there are some tricks to know
involving how to define these relations. I am excited for the day when AI
tooling can be my partner in finding these proofs, especially if I can get hints
on where might be a fruitful direction to go. Or even help removing boilerplate
proof code would be nice!


## Comments, questions?

If you have any comments or questions, feel free to do one of the following.

- Contact me at my email, which is this web address with the `.` after ryan replaced with an `@`.
- Start a [discussion on my github page](https://github.com/ryanorendorff/ryanorendorff.github.io/discussions).
- Say hi on [LinkedIn](https://www.linkedin.com/in/ryan-orendorff/)


## Acknowledgements

I would like to thank  [Spencer Poff](http://spencerpoff.com/) for reviewing
drafts of this article.


## Want to run the code in this blog post?

This blog post is a literate Agda file, meaning you too can check that ChatGPT
got the right answer! To spin up a nix shell that can load this file, simply run
`nix-shell` in [this post's
directory](https://github.com/ryanorendorff/ryanorendorff.github.io).

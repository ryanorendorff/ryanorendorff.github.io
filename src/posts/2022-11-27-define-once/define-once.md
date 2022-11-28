---
title: Define a python function once and for all
subtitle: A terrible hack that ends up being surprisingly useful
---

Sometimes when we have a function that takes a long time to run, and we would
really rather avoid running the function more often than absolutely necessary.
Luckily, if our function is a pure function[^1], then we can
[memoize](https://en.wikipedia.org/wiki/Memoization) the result of calling the
function using the `cache` function.

[^1]: The function's inputs and outputs form a relation and the function does
      no side effects such as print out to standard out.

```python
from functools import cache

@cache
def this_function_takes_forever(*args, **kwargs):
    # do some long computation
    return result
```

This is an excellent way to reduce compute time when making a repeated calls to
a function with the same inputs. Excited with this technique, we implement some
long running function in a [Jupyter notebook](https://jupyter.org/) and slap the
`cache` decorator on top to memoize the results.

```python
@cache
def long_running_notebook_function(*args, **kwargs):
    # Now our notebook will no longer take forever to run!
    return result
```

We use this new function with reckless abandon in our code, and it significantly
reduce the time it takes a cell in the notebook to run. At some point we want to
rerun everything in the notebook (to say regenerate some plots with new
`matplotlib.rcParams` settings), and we use the "Run All Cells" menu item

...

and it now takes _forever_ to run the same cells that. What happened?!


## Functions are redefined every time a cell is run

When using a notebook, the code in a cell is rerun every time we execute that
cell. The definitions in the cell being run are assigned to the global namespace
in the running notebook kernel. Put another way, we can think of running a cell
as the following pseudocode.

```python
# Before running a cell, we can find find all globally defined
# variables in the `globals()` dictionary. This will include
# a pointer to all of our defined functions, plus a bunch of
# stuff defined by the Python kernel/Jupyter notebook internals.
before = globals()

# Take the source code in the cell as a string called `cell_str`.
# Anything defined in the cell string will be merged into the 
# `globals()` dictionary.
exec(cell_str)

# The globals dictionary should now be modified with whatever was
# defined within `cell_str`.
after = globals()

# The following equation relates the two dictionaries
# (minus some jupyter bookkeeping)
#
#     after = before | top_level_definitions(cell_str)
#
# where `|` is the dictionary merge operator from Python 3.9 and
# `top_level_defintions` returns the top level definitions in the
# string along with the associated function pointers created by `exec`.
```

The reason the `cache` decorator does not seem to work is that when we run a
cell, a _new function with the same name_ gets created and replaces the existing
function in the global namespace. We lose track of our first cached function!


## But all is not lost!

For the `cache` decorator to working even after running the cell that defines
the desired function multiple times, we need to keep track of the original
function with the cache attached and ignore the new function created when the
cell is run again. Conveniently, the `globals()` dictionary gives us access to
the global namespace, enabling us to see if a function with a given name already
exists and hold onto the original definition instead of the new (likely
equivalent) definition.  The name of a function can be looked up through the
`__name__` property, so we can define the following decorator[^2] to find the
existing function and keep it.

[^2]: A decorator is just a higher order function (a function that takes another
      function as an argument).

```python
def define_once_first_try(f):
    if f.__name__ in globals().keys():
        # Return the original function, ignoring the new
        # definition of `f`.
        return globals()[f.__name__]
    
    # Otherwise, the function does not already exist and we should
    # assign this new function to the global namespace.
    return f 
```

Now we can decorate our function with `define_once_first_try` once it is
memoized[^3] to prevent subsequent reruns of a cell from clearing out our cache.

[^3]: The order of decorators actually does not matter; `cache` is smart enough
      to notice that a cache for the specific function already exists.

```python
@define_once_first_try
@cache
def long_running_notebook_function(*args, **kwargs):
    # Now our notebook will no longer take forever to run!
    return result
```

Now we can go back to merrily coding in our notebook, using "Run All Cells"
recklessly without worrying about recomputing expensive results.

At some point, we realize our `long_running_notebook_function` has a bug. No
problem, we can go back and fix the function and go back to the task at hand.

```python
@define_once_first_try
@cache
def long_running_notebook_function(*args, **kwargs):
    # Now our notebook will no longer take forever to run!
    
    # We were missing a constant 5 from our results. Fixed now!
    return result + 5
```

We rerun the cell defining `long_running_notebook_function` and all the cells
that need this function. But we are getting the same resulting buggy results as
before. Hmm, what is going on?


## Since we only define a function once, we can't modify it!

Our strategy for preventing a function from being redefined is a tad too strong:
it only keeps track of the first definition of the code, meaning we can never
alter that definition. It would be amazing if we could write perfect code so
that we would not need to redefine a function, but alas we are human. What the
`define_once` function should really be doing is detecting _if the function
meaningfully changes_, and if it does, to accept the new definition
(invalidating the old cache in the process).

We can get a function's source code using the `inspect.getsource` function from
the standard library and use the source code to see if the function changed in a
meaningful way. If the code has not changed, then we should return the original
pointer to the function (with the cache attached); otherwise we should use the
new function definition.

```python
import inspect

def define_once_second_try(f):
    if f.__name__ in globals().keys():
        f_source = inspect.getsource(f)
        global_f_source = inspect.getsource(globals()[f.__name__])

        if f_source == global_f_source:
            # Return the original function, ignoring the new definition
            # of `f` ONLY IF the source code is unchanged.
            return globals()[f.__name__]
    
    # Otherwise, the function does not already exist with the same
    # source code and we should assign the function to the
    # global namespace.
    return f 
```

With our second attempt, we can fix any bugs in our code and keep the cached
function whenever we do not alter the function. Great!

With our new define once function, we get back to our task (_sheesh, took a
while_). After a bit of coding and finalizing our work, we decide to run the
notebook through an autoformatter like [`black`](https://github.com/psf/black),
add some comments,  and rerun all the cells. But now our code is taking a long
time to run again!  What's the problem now?


## Source code includes formatting details that don't affect how a function runs

Our new problem is that the function source code includes both the instructions
of what to run and the details required to make the code readable to humans.
Things such as [tabs versus spaces](https://www.youtube.com/watch?v=SsoOG6ZeyUI)
don't change what the function actually does, but changes what the program looks
like on our computer screens. We can make changes to a function such as adding
comments that do not _meaningfully_ change a function, but change the source
code nonetheless.

What we would really like is a representation of only the computational bits of
the function, which we could then use to compare the existing implementation of
a function from the redefinition from a function.  Luckily we can extract the
[abstract syntax tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
for a function in python using the `ast` module in the standard library. The AST
is the data structure that represents the program after it has been parsed from
its text origins, retaining only the computationally relevant bits.

We can define our `define_once` function to compare the AST of a functions and
its redefinition and only accept the redefinition if it does something
meaningfully different.

```python
def function_to_ast_string(f):
    # Get the AST in tree form
    node = ast.parse(inspect.getsource(f))
    
    # Convert to string. There is no comparison function for AST
    # nodes in the standard library, but the function to convert
    # a given AST to a string representation is deterministic
    # (and each AST has a unique string).
    return ast.dump(node)

def define_once(f):
    if f.__name__ in globals().keys():
        f_ast = function_to_ast_string(f)
        global_f_ast = function_to_ast_string(globals()[f.__name__])

        if f_ast == global_f_ast:
            # Return the original function, ignoring the new
            # definition of `f` ONLY IF the AST is unchanged.
            return globals()[f.__name__]
    
    # Otherwise, the function does not already exist with the same
    # AST and we should assign the function to the global namespace.
    return f 
```

And with this version of `define_once` we can finally 

- use the advantages of a stateful decorator like `cache` in our Python
  notebook, even after rerunning a cell,
- reduce the number of cases where we have no cached result, and
- are immune to syntactic changes causing a cache miss.


## Is there a more general way to detect a "meaningful" change is?

A natural question after using the AST to compare two functions is whether this
method also redefines a function too often, much like our source code comparison
method was too strict with its understanding of what a "meaningful" change was.
Is there a more general definition of a "meaningful" change that we should be
looking for? 

One more general definition is if our original function and its redefinition
always return the same output for a given input, then we can consider no
meaningful change has been made in the redefinition. For example, we can define
a function in two ways that produce the same result.

```python
def definition_one(a: int) -> int:
    return a + 5

def definition_two(a: int) -> int:
    return 5 + a
```

Since these two definitions should generate the same cache, we can consider them
equivalent. The  This definition of equality is called [function
extensionality](https://en.wikipedia.org/wiki/Extensionality): if for all inputs
$x$ we can show that, for two functions $f$ and $g$ that $f(x) = g(x)$, then we
can say that $f$ and $g$ are extensionally equal. Our AST comparison we used for
function equivalence was an _intensional equality_: two functions were equal if
they were syntactically equal for a given definition of syntaxes being equal. In
this case, we defined the syntaxes as being equal if the AST was the same.

Function extensionality seems to be a good way to decide if two functions are
equivalent, so why did we not use that? Well there are two main reasons I can
think of. The simpler problem is that if we redefine the function to be far more
computationally efficient, then we would be ignoring this more efficient
definition since the input/output mapping is the same.

More importantly though, in general it is difficult to determine if two
functions are extensionally equivalent except for in very specific cases,
especially in languages such as Python[^4].  One could attempt to recognize some
ASTs as equivalent through rules that recognize some equivalent transformations.
For example, a rule could detect that `a + b` is the same as `b + a`. It would
be difficult to come up with a set of rules that encapsulated the right
definition of a meaningful difference between two ASTs, likely taking far more
time than just rerunning the expensive computation. In addition, I would argue
that when we edit code that changes the AST that we are usually making
meaningful changes, so we are unlikely to redfine a function more than we really
want to in practice.

[^4]: There are some programming languages such as
      [Dhall](https://dhall-lang.org/) which have a more generic way of comparing
      functions through strong normalization.

Should one really want to extend the definition of equality, then
[Satisfiability modulo theories
(SMT)](https://en.wikipedia.org/wiki/Satisfiability_modulo_theories)  solvers
can be used to symbolically reason about programs. Using such tools, one can
determine equality of two functions by asking if there is some [symbolic
assignment](https://en.wikipedia.org/wiki/Symbolic_execution) applied to both
functions that returns an unequal result. Tools such as
[Crux](https://crux.galois.com/) from Galois or the work from the [Languages,
Systems, and Data Lab @ UCSC](https://lsd.ucsc.edu/) use this technique to prove
that a function implements a certain specification, such as [producing
equivalent outputs as a less efficient
function](https://www.youtube.com/watch?v=dCNQFHjgotU).  Normal testing is
insufficient in this case as it is often not possible to try every input to a
function, meaning it is possible to miss a bug arising from an edge case.[^5]

[^5]: Thanks Patrick Redmond for suggesting expanding the section on ways to
      evaluate function equivalence.


## Should we do this?

This approach to preserving a cache is definitely a hack: we are altering how
functions are assigned to a global namespace. Anytime code that pulls out the
`ast` module, the `inspect` module, or the `globals` function is likely doing
something that is probably gross—if we are using all three, then we should
definitely rethink what we are doing! This idea should not be used in  actual
production code.

However, for prototyping in notebooks this method enables us to quickly enable
decorators with state (such as `cache`) without trying a more canonical solution
such as persistent storage. I personally use this method in notebooks and then
delete the `define_once` decorator once the code is moved into a module.


## Comments, questions?

If you have any comments or questions, feel free to do one of the following.

- Contact me at my email, which is this web address with the `.` after ryan replaced with an `@`.
- Start a [discussion on my github page](https://github.com/ryanorendorff/ryanorendorff.github.io/discussions).
- Say hi on [LinkedIn](https://www.linkedin.com/in/ryan-orendorff-153a45ba/)


## Acknowledgements

I would like to thank [Patrick Redmond](https://curious.software/plr/) and
[Spencer Poff](http://spencerpoff.com/) for reviewing drafts of this article.


## Want to run the code in this blog post?

The code defined in this post is available in a [Jupyter
notebook](https://github.com/ryanorendorff/ryanorendorff.github.io/blob/main/src/posts/2022-11-27-define-once/define-once.ipynb).
A python environment with Jupyter notebook can be spun up using [nix
shell](https://nixos.wiki/wiki/Development_environment_with_nix-shell) in this
posts [root
directory](https://github.com/ryanorendorff/ryanorendorff.github.io/blob/main/src/posts/2022-11-27-define-once/define-once.ipynb)

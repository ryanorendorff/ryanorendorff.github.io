---
title: Define a python function once and for all
subtitle: A terrible hack that ends up being surprisingly useful
---

Sometimes when we have a function that takes a long time to run, and we would
really rather attempt to avoid running the function more than necessary.
Luckily, if our function a pure function[^1], then we can memoize the result of
the function given some inputs using `cache`.

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
long running function in a Jupyter notebook and slap the `cache` decorator
on top to memoize the results.

```python
@cache
def long_running_notebook_function(*args, **kwargs):
    # Now our notebook will no longer take forever to run!
    return result
```

We use this new function with abandon within our code, and it significantly
reduce the time it takes the cells in the notebook to run. At some point we want
to rerun everything in the notebook (to say regenerate some plots with new
`matplotlib.rcParams` settings), and we use the "Run All Cells" menu item

<!--
Run cell ▶ 
Run all ▸▸
-->

...

and it now takes _forever_ to run the same cells that. What happened?!


## Functions are redefined every time a cell is run

When using a notebook, the code in a cell is rerun every time we execute that
cell. The results are all assigned to the global namespace in the running
notebook kernel. Put another way, we can think of running a cell as the
following pseudocode.

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

For the `cache` decorator to continue working even after running a cell, we
need to keep track of the function with the cache attached and not lose track of
it when running a cell. Conveniently, the `globals()` dictionary gives us access
to the global namespace, enabling us to see if a function with a given name
already exists and hold onto the original definition instead of redefining the
function. The name of a function can be looked up through the `__name__`
property, so we can define the following generator to find the existing function
and keep it.

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
memoized[^2] to prevent subsequent reruns of a cell from clearing out our cache.

[^2]: The order of decorators actually does not matter; `cache` is smart enough
      to notice that a cache for the specific function already exists.

```python
@define_once_first_try
@cache
def long_running_notebook_function(*args, **kwargs):
    # Now our notebook will no longer take forever to run!
    return result
```

Now we can go back to merrily coding in our notebook, using "Run All Cells"
recklessly without worry of recomputing expensive results.

At some point, we realized our `long_running_notebook_function` has a bug. No
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
definition changes_, and if it does, to accept the new definition.

We can get a function's source code using the `inspect.getsource` function from
the standard library and use the source code to see if the function changed. If
the code has not changed, then we should return the original pointer to the
function (with the cache attached); otherwise we should use the new function
definition.

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
like on our computer screens. What we would really like is a representation of
only the computational bits of the function, which we could then use to compare
the existing implementation of a function from the redefinition from a function.

Luckily we can extract the [abstract syntax tree
(AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree) for a function in
python using the `ast` module in the standard library. The AST is the data
structure that represents the program after it has been parsed from its text
origins, retaining only the computationally relevant bits.

We can define our `define_once` function to compare the AST of a functions and
its redefinition and only accept the redefinition if it is does something
computationally different.

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


## Is there an even more generic version of functioon equivalence?

A natural question after using the AST to compare two functions is whether this
is the most generic way to do the comparison. For example, we can define a
function in two ways that produce the same result

```python
def definition_one(a: int) -> int:
    return a + 5

def definition_two(a: int) -> int:
    return 5 + a
```

These functions are equivalent in terms of how the inputs map to the outputs, so
in some manner we can say that the [functions are
equal](https://en.wikipedia.org/wiki/Extensionality). However, in general it is
difficult to determine if two functions are equivalent except for in very
specific cases, especially in languages such as Python[^3]. One could attempt to
recognize some ASTs as equivalent through rules such as `a + b` is the same as
`b + a`, but I believe that these types of checks are not useful in practice.
We are far more often making computationally meaningful changes to a function
instead of changes that preserve the exact computation.

[^3]: There are some programming languages such as
      [Dhall](https://dhall-lang.org/) which have a more generic way of comparing
      functions through strong normalization.


## Should we do this?

This approach to preserving a cache is definitely a hack: we are altering how
functions are assigned to a global namespace. One should not use technique in
actual production code. However, for prototyping in notebooks this method
enables us to quickly enable decorators with state (such as `cache`) without
trying a more canonical solution such as persistent storage. I personally use
this method in notebooks and then delete the `define_once` decorator once the
code is moved into a module.


## Comments, questions?

If you have any comments or questions, feel free to do one of the following.

- Contact me at my email, which is this web address with the `.` after ryan replaced with an `@`.
- Open an [issue on my github page](https://github.com/ryanorendorff/ryanorendorff.github.io).
- Say hi on [LinkedIn](https://www.linkedin.com/in/ryan-orendorff-153a45ba/)


## Acknowledgements

I would like to thank Spencer Poff for reviewing drafts of this article.


## Want to run the code in this blog post?

The code defined in this post is available in a [Jupyter
notebook](https://github.com/ryanorendorff/ryanorendorff.github.io/blob/main/src/posts/2022-11-27-define-once/define-once.ipynb).
A python environment with Jupyter notebook can be spun up using [nix
shell](https://nixos.wiki/wiki/Development_environment_with_nix-shell) in this
posts [root
directory](https://github.com/ryanorendorff/ryanorendorff.github.io/blob/main/src/posts/2022-11-27-define-once/define-once.ipynb)

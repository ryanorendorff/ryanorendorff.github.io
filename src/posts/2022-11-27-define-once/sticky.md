---
title: Make a python function definition stick!
subtitle: A terrible hack that ends up being surprisingly useful
---

Sometimes we have a function that takes a while, and we want to use just-in-time
compilation to make the function faster.
[Numba](https://numba.readthedocs.io/en/stable/user/jit.html) will compile the
function lazily in this case: only when it is first called will the function be
optimized.

```python
from numba import jit

@jit
def function_that_takes forever(*args, **kwargs):
    # do some long computation
    return result
```

This is an excellent way to reduce compute time when making a repeated calls to
slow function. Excited with this technique, we implement some long running
function in some type of Python REPL (my favorite at the moment is ptpython) we
are analyzing data in and slap the `jit` decorator on top.

```python
@jit
def long_running_repl_function(*args, **kwargs):
    # Now our notebook will no longer take forever to run!
    return result
```

We use this new function with reckless abandon in our code, and it significantly
reduce the time it takes our code to run in our REPL. At some point we press the
up arrow to get to our old blocks of code, rerun then with enter, and...

and it now takes _forever_ to run the function the first time.  What happened?!


# Functions are redefined every time

When using a REPL, the code in a block is rerun every time we execute that
block. The definitions in the block being run [are assigned to the global
namespace in the running python
interpreter](https://www.mikulskibartosz.name/python-memory-management-in-jupyter-notebook/).
Put another way, we can think of running a block as the following pseudocode.

```python
# Before running a block, we can find all globally defined
# variables in the `globals()` dictionary. This will include
# a pointer to all of our defined functions, plus potentially a
# bunch of stuff that our REPL may keep track of like `In` and
# `Out` blocks in something like ipython or ptpython.
before = globals()

# Take the source code in the block as a string called `block_str`.
# Anything defined in the block string will be merged into the 
# `globals()` dictionary.
exec(block_str)

# The globals dictionary should now be modified with whatever was
# defined within `block_str`.
after = globals()

# The following equation relates the two dictionaries
# (minus some jupyter bookkeeping)
#
#     after = before | top_level_definitions(block_str)
#
# where `|` is the dictionary merge operator from Python 3.9 and
# `top_level_defintions` returns the top level definitions in the
# string along with the associated function pointers created by `exec`.
```

The reason the `jit` decorator does not seem to work is that when we run a block,
a _new function with the same name_ gets created and replaces the existing
function in the global namespace. We lose track of our first jitted function!


# But all is not lost!

For the `jit` decorator to working even after running the block that defines
the desired function multiple times, we need to keep track of the original
function with the jit attached and ignore the new function created when the
block is run again. Conveniently, the `globals()` dictionary gives us access to
the global namespace, enabling us to see if a function with a given name already
exists and hold onto the original definition instead of the new (likely
equivalent) definition. The name of a function can be looked up through the
`__name__` property, so we can define the following decorator[^1] to find the
existing function and keep it.

[^1]: A decorator is just a higher order function (a function that takes another
      function as an argument).

```python
def sticky_definition_first_try(f):
    # Here we try to find the function in the global
    # namespace. If it does not exist, return the 
    # newly defined function.
    return globals().get(f.__name__, f)
```

Now we can decorate our function with `sticky_definition_first_try` once it is
jitted to prevent subsequent reruns of a block from clearing out our optimized
function.


```python
@sticky_definition_first_try
@jit
def long_running_repl_function(*args, **kwargs):
    # Now our notebook will no longer take forever to run!
    return result
```

Now we can go back to merrily coding in our REPL without worrying about
recomputing expensive results.

At some point, we realize our `long_running_repl_function` has a bug. No
problem, we can go back and fix the function and go back to the task at hand.

```python
@sticky_definition_first_try
@jit
def long_running_repl_function(*args, **kwargs):
    # Now our notebook will no longer take forever to run!
    
    # We were missing a constant 5 from our results. Fixed now!
    return result + 5
```

We rerun the block defining `long_running_repl_function` and all the blocks
that need this function. But we are getting the same resulting buggy results as
before. Hmm, what is going on?


# Since we only define a function once, we can't modify it!

Our strategy for preventing a function from being redefined is a tad too strong:
it only keeps track of the first definition of the code, meaning we can never
alter that definition. It would be amazing if we could write perfect code so
that we would not need to redefine a function, but alas we are human. What the
`sticky_definition` function should really be doing is detecting _if the function
meaningfully changes_, and if it does, to accept the new definition
(invalidating the old jit in the process).

What we would really like is a representation of only the computational bits of
the function, which we could then use to compare the existing implementation of
a function from the redefinition from a function. Luckily we can extract the
[abstract syntax tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
for a function in python using the `ast` module in the standard library. The AST
is the data structure that represents the program after it has been parsed from
its text origins, retaining only the computationally relevant bits.

We can define our `sticky_definition` function to compare the AST of a functions and
its redefinition and only accept the redefinition if it does something
meaningfully different[^2] (with generous help from StackOverflow).

[^2]: See references for examples of performing this type of AST comparison.

```python
import ast
import inspect
import hashlib

# From https://stackoverflow.com/questions/49998161/how-can-i-hash-the-body-of-a-python-function
# with light editing
def _remove_docstring(node):
    if not (isinstance(node, ast.FunctionDef) or
            isinstance(node, ast.ClassDef)):
        return

    if len(node.body) != 0:
        docstr = node.body[0]
        if (isinstance(docstr, ast.Expr) and
            isinstance(docstr.value, ast.Str):
            node.body.pop(0)


# From https://stackoverflow.com/questions/49998161/how-can-i-hash-the-body-of-a-python-function
# with light editing
def hash_function(func):
    func_str = inspect.getsource(func)
    module = ast.parse(func_str)

    assert (len(module.body) == 1 and
            isinstance(module.body[0], ast.FunctionDef))
    
    # Clear all the doc strings
    for node in ast.walk(module):
        _remove_docstring(node)

    # Convert the ast to a string for hashing
    ast_str = ast.dump(module, annotate_fields=False)

    # Produce the hash
    fhash = hashlib.sha256(ast_str)
    result = fhash.hexdigest()
    return result


def sticky_definition(f):
    # function was not defined before, just return the new function.
    if f.__name__ not in globals():
        return f

    global_f = globals()[f.__name__]

    if hash_function(f) == hash_function(global_f)
        # Return the original function, ignoring the new definition
        # of `f` ONLY IF the source code is unchanged.
        return global_f
    else:
        # Otherwise, the function does not already exist with the same
        # source code and we should assign the function to the
        # global namespace.
        return f 
```

And with this version of `sticky_definition` we can

- use the advantages of a stateful decorator like `jit` in our Python
  REPL, even after rerunning a block,
- reduce the number of cases where we have no jitted result, and
- are immune to syntactic changes like docstrings or comments causing a jit
  miss.


# Is there a more general way to detect a "meaningful" change?

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

Since these two definitions should generate the same jit, we can consider them
equivalent. This definition of equality is called [function
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
especially in languages such as Python[^3]. One could attempt to recognize some
ASTs as equivalent through rules that recognize some equivalent transformations.
For example, a rule could detect that `a + b` is the same as `b + a`. It would
be difficult to come up with a set of rules that encapsulated the right
definition of a meaningful difference between two ASTs, likely taking far more
time than just rerunning the expensive computation. In addition, I would argue
that when we edit code that changes the AST that we are usually making
meaningful changes, so we are unlikely to redfine a function more than we really
want to in practice.

[^3]: There are some programming languages such as
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
function](https://www.youtube.com/watch?v=dCNQFHjgotU). Normal testing is
insufficient in this case as it is often not possible to try every input to a
function, meaning it is possible to miss a bug arising from an edge case.[^4]

[^4]: Thanks Patrick Redmond for suggesting expanding the section on ways to
      evaluate function equivalence.


# Should we do this?

This approach to preserving a jit is definitely a hack: we are altering how
functions are assigned to a global namespace. Anytime code that pulls out the
`ast` module, the `inspect` module, or the `globals` function is likely doing
something that is probably gross—if we are using all three, then we should
definitely rethink what we are doing! This idea should not be used in actual
production code.

However, for prototyping (including in places like a REPL) this method enables
us to quickly enable decorators with state (such as `jit`) without trying a
more canonical solution such as persistent storage. Sometimes coming up with
that robust saving state challenge can take quite a while in its own right!


# References

This type of idea has been done a few different times in the public literature.

- There are [many](https://stackoverflow.com/a/321334)
  [ways](https://stackoverflow.com/questions/3948873/prevent-function-overriding-in-python)
  to
  [accomplish](https://stackoverflow.com/questions/31700406/how-to-prevent-overwritting-python-built-in-function-by-accident)
  this
  [task](https://stackoverflow.com/questions/49998161/how-can-i-hash-the-body-of-a-python-function),
  including methods that compare [function
  contents](https://stackoverflow.com/questions/32287885/caching-functions-in-python-to-disk-with-expiration-based-on-version).
  There are other methods for mucking with [globals lookup inside a
  function](https://stackoverflow.com/questions/49076566/override-globals-in-function-imported-from-another-module),
  including [disabling functions deep inside other
  code](https://stackoverflow.com/questions/10388411/possible-to-globally-replace-a-function-with-a-context-manager-in-python).
  In general all of these methods should be used _very carefully_ and likely not
  in production code (as every stack overflow post mentions).
- In a fascinating twist, one [python bug
  report](https://bugs.python.org/issue13678) asked for a similar prevention of
  overriding implemented in Python itself! 
- There is a [python package](https://github.com/omegacen/python-compare-ast)
  for comparing ASTs.
- There is a
  [thesis](https://repositorio.uniandes.edu.co/bitstream/handle/1992/44754/u830947.pdf?sequence=1)
  discussing comparing many more methods for comparing Python ASTs.


## Comments, questions?

If you have any comments or questions, feel free to do one of the following.

- Contact me at my email, which is this web address with the `.` after ryan replaced with an `@`.
- Start a [discussion on my github page](https://github.com/ryanorendorff/ryanorendorff.github.io/discussions).
- Say hi on [LinkedIn](https://www.linkedin.com/in/ryan-orendorff/)


## Acknowledgements

I would like to thank [Patrick Redmond](https://curious.software/plr/) and
[Spencer Poff](http://spencerpoff.com/) for reviewing drafts of this article.


## Want to run the code in this blog post?

A python environment can be spun up using [nix
shell](https://nixos.wiki/wiki/Development_environment_with_nix-shell) in this
posts [root
directory](https://github.com/ryanorendorff/ryanorendorff.github.io/blob/main/src/posts/2022-11-27-define-once).
Play around with different definitions for comparing functions and see what
kinds of tradeoffs you encounter.

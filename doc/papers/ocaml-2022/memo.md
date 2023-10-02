---
geometry: "left=2cm,right=2cm,top=2.4cm,bottom=2.1cm"
bibliography: refs.bib
csl: refs.csl
---

# Memo: an incremental computation library that powers Dune

Andrey Mokhov, Arseniy Alekseyev

*Jane Street, London, United Kingdom*

### Abstract

We present Memo, an incremental computation library that supports a new, faster
and more scalable, file-watching build mode in Dune 3.0. The requirements from
the build systems domain make Memo a unique point in the design space of
incremental computation libraries. Specifically, Memo needs to cope with
concurrency, dynamic dependencies, dependency cycles, and non-determinism;
provide support for efficiently collecting and reporting user-friendly errors;
and scale to computation graphs containing tens of millions of incremental
nodes.

## Introduction

The OCaml build system Dune [@dune] supports a continuous file-watching build
mode, where rebuilds are triggered automatically as the user is editing source
files. A simple and naive way to implement this functionality is to restart Dune
on any file change but that is too slow for large projects. To avoid re-scanning
the project's source tree, re-parsing all build specification files, and
re-generating all build rules from scratch every time, Dune uses an incremental
computation library called Memo. Below we briefly introduce the Memo's API.

Memo provides a monadic API built on top of the *structured concurrency monad*
`Fiber`. Dune uses `Fiber` to speed up builds by executing external commands in
parallel, e.g., running multiple instances of `ocamlopt` to compile independent
source files. To inject a *fiber* into the `Memo` monad, and to extract it back,
one can use the following pair of functions:

```ocaml
  val of_fiber : 'a Fiber.t -> 'a Memo.t
  val run : 'a Memo.t -> 'a Fiber.t
```

More interestingly, functions in the `Memo` monad can be memoized and cached
between different build runs[^1]:

[^1]: The function `create` takes a few more arguments, e.g., for reporting good
error messages, which we omit for the sake of clarity.

```ocaml
  val create : ('i -> 'o Memo.t) -> ('i, 'o) Memo.Table.t
  val exec : ('i, 'o) Memo.Table.t -> 'i -> 'o Memo.t
```

Here `Memo.Table.t` is a table that stores the input/output mapping computed in
the current run, along with the *dependencies* that are automatically captured
when memoized functions call one another. Such explicit memoization, instead of,
e.g., memoizing every `Memo.map` and `Memo.bind` call, makes it easy to control
the degree of incrementality.

Finally, Memo provides a way to *invalidate* a specific input/output pair, or a
*cell*, via the following API:

```ocaml
  val cell : ('i, 'o) Memo.Table.t -> 'i -> ('i, 'o) Cell.t
  val invalidate : ('i, 'o) Cell.t -> Invalidation.t (* [Invalidation.t]s can be combined *)
  val restart : Invalidation.t -> unit
```

When Dune receives new events from the file-watching backend, it cancels the
build run by interrupting the currently running external commands (if any), and
then calls `restart` to let Memo know which files changed and initiate a new
build run. When evaluating future calls to `run`, Memo will consider all outputs
that transitively depend on the invalidated cells as out of date, and will
recompute them when/if needed.

## Key features

This section discusses the most interesting features of Memo and some aspects of
the implementation.

Firstly, to *capture dependencies*, Memo maintains a call stack, where stack
frames correspond to `exec` calls. The call stack is also used for reporting
good error messages: when a user-supplied function raises an exception, we
extend it with the current Memo stack trace using human-readable annotations
provided to `create` via an optional argument.

Memo supports two ways of *error reporting*: *early* (to show errors to the user
as soon as they occur during a build), and *deterministic* (to provide a stable
error summary at the end of the build). To speed up rebuilds, Memo also supports
*error caching*: by default, it doesn't recompute a `Memo` function that
previously failed if its dependencies are up to date. This behaviour can be
overridden for *non-reproducible errors* that should not be cached, for example,
the errors that occur while Dune cancels the current build by interrupting the
execution of external commands.

One of the most interesting features of Memo compared to other incremental
computation libraries is *dependency cycle detection*.
Since Dune supports *dynamic build dependencies* [@mokhov2020build], the
dependency graph is not known before the build starts. During the build, new
computation nodes and dependency edges are discovered concurrently, and Memo
uses an incremental cycle detection algorithm [@gueneau2019cycles] to detect and
report dependency cycles as soon as they are created. This is a unique feature
of Memo: Incremental [@incremental] and Adapton [@hammer2014adapton] libraries
do not support concurrency; the Tenacious library (used by Jane Street's
internal build system Jenga) does support concurrency but it detects cycles by
"stopping the world" and traversing the "frozen" dependency graph to see if
concurrent computations might have deadlocked by waiting for each other. The
approach used by Tenacious is conceptually simple but introduces a delay between
the creation of a dependency cycle and its detection. Memo reports dependency
cycle errors without delay, and uses the human-readable annotations supplied to
`create` to make it easier to understand and debug cycles.

Compared to the incremental computation libraries mentioned above, the current
implementation of Memo makes one unusual design choice. Incremental, Adapton and
Tenacious are all *push based*, i.e., they trigger recomputation starting from
the leaves of the computation graph. Memo is *pull based*: `invalidate` calls
merely mark leaves (cells) as out of date, but recomputation is driven from the
top-level `run` calls. The main drawback of our approach is that Memo traverses
the whole graph on each rebuild. The main benefits are: (i) Memo
doesn't need to store reverse dependencies, which saves space and eliminates
various garbage collection pitfalls; (ii) the pull based approach makes it
easier to avoid "spurious" recomputations, where a node is recomputed but
subsequently becomes unreachable from the top due to a new dependency structure.
Our current experiments show that traversing the whole graph on each rebuild
isn't prohibitively expensive even for large builds where computation graphs
contain tens of millions of nodes. We may rethink this design decision in future
as Dune and Memo need to scale to larger projects. Having said that, we consider
the pull based approach to incremental computation to be under-researched, and
are keen to investigate how far we can take it in practice.

## Development status

Memo is still in active development and we welcome feedback from the OCaml
community on how to make it better. While the current implementation is tied to
Dune's lightweight concurrency library Fiber, the core functionality can be made
available as a functor over an arbitrary concurrency monad, making it usable
with Async and Lwt.

## Acknowledgements

We thank Jeremie Dimino for driving the design of Memo and for his work on
incrementalising Dune. We are also grateful to Rudi Horn, Rudi Grinberg, Emilio Jesús Gallego
Arias and other Dune developers for their many contributions, and to Armaël
Guéneau for helping us integrate the incremental cycle detection library in
Memo.

# References

<!-- References to be generated by Pandoc. -->

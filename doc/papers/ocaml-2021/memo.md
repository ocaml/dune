---
geometry: "left=2cm,right=2cm,top=2.4cm,bottom=2.1cm"
bibliography: refs.bib
csl: refs.csl
---

# Memo: the incremental computation library that powers Dune

Andrey Mokhov, Arseniy Alekseyev, Jeremie Dimino

*Jane Street, London, United Kingdom*

### Abstract

We present Memo, an incremental computation library that improves Dune's
performance on large-scale code bases. The requirements that come from the build
systems domain make Memo an interesting point in the design space of incremental
computation libraries. In particular, Memo needs to cope with concurrency,
dynamic dependencies, dependency cycles, non-determinism, and scale to
computation graphs containing millions of nodes.

## Introduction

The OCaml build system Dune [@dune] supports a continuous file-watching build
mode, where rebuilds are triggered automatically as the user is editing source
files. The simplest way to implement this functionality is to restart Dune on
any file change but that is too slow for large projects. To avoid re-scanning
the project's file tree, re-parsing all build specification files, and
re-generating all build rules from scratch every time, Dune uses an incremental
computation library called Memo. Below we briefly introduce the Memo's API.

Memo's `Memo` monad is implemented on top of the *concurrency monad* `Fiber`.
Dune uses `Fiber` to speed up builds by executing external commands in parallel,
e.g., running multiple instances of `ocamlopt` to compile independent source
files. To inject a *fiber* into the `Memo` monad, and to extract it back, one
can use the following pair of functions:

```ocaml
  val of_reproducible_fiber : 'a Fiber.t -> 'a Memo.t
  val run : 'a Memo.t -> 'a Fiber.t
```

More interestingly, functions in the `Memo` monad can be memoized and cached
between different build runs:

```ocaml
  val create : ('i -> 'o Memo.t) -> ('i, 'o) Memo.Table.t (* A few arguments omitted for simplicity *)
  val exec : ('i, 'o) Memo.Table.t -> 'i -> 'o Memo.t
```

Here `Memo.Table.t` is a memoization table that stores the input/output mapping
computed in the current build run. Outputs are stored alongside their *dependencies*,
which are automatically captured by Memo when memoized functions call one another.
Finally, Memo provides a way to *invalidate* a specific input/output pair, or a
*cell*, via the following API:

```ocaml
  val cell : ('i, 'o) Memo.Table.t -> 'i -> ('i, 'o) Cell.t
  val invalidate : ('i, 'o) Cell.t -> unit
  val restart_current_run : unit -> unit
```

When Dune receives new events from the file-watching backend, it invalidates the
cells that correspond to the files that have changed. It then cancels the build
by interrupting the currently running external commands (if any), and finally
calls `restart_current_run` to notify Memo that a new build run has started and
hence all outputs that transitively depend on the invalidated cells should now
be considered out of date.

## Implementation notes

The above introduction covers only a small part of the Memo's API, yet it gives
us enough context to discuss the most interesting aspects of the implementation.

Firstly, to *capture dependencies*, Memo maintains a call stack, where stack
frames correspond to `exec` calls. The call stack is also useful for reporting
good error messages: when a user-supplied function raises an exception, we
extend it with the current Memo stack trace using human-readable annotations
provided to `create` via an optional argument.

Memo supports two ways of *error reporting*: *early* (to show errors to the user
as soon as they occur during a build), and *deterministic* (to provide a stable
error summary at the end of the build). To speed up rebuilds, Memo also supports
*error caching*: by default, it doesn't recompute a `Build` function that
previously failed if its dependencies are up to date. This behaviour can be
overridden for *non-reproducible errors* that should not be cached, for example,
the errors that occur while Dune cancels the current build by interrupting the
execution of external commands. Another source of complexity when dealing with
errors is that memoized functions can form diamond-shaped call graphs, so to
avoid reporting the same error multiple times, Memo needs to deduplicate them.

<!-- aalekseyev says: diamond-shaped call graphs introduce a lot of complexity
     in Memo implementation, so singling out error handling may be misleading. -->

One of the most interesting aspects of Memo is *dependency cycle detection*.
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
the creation of a dependency cycle and its detection.

Compared to the incremental computation libraries mentioned above, the current
implementation of Memo makes one controversial design choice. Incremental,
Adapton and Tenacious are all *push based*, i.e., they trigger recomputation
starting from the leaves of the computation graph. Memo is *pull based*:
`invalidate` calls merely mark leaves (cells) as out of date, but recomputation
is driven from the top-level `run` calls. The main drawback of this approach is
that Memo needs to traverse the whole graph on each rebuild. The main benefits
are: (i) Memo doesn't need to store reverse dependencies, which saves space and
eliminates various garbage collection pitfalls; (ii) the pull based approach
makes it easier to avoid "spurious" recomputations, where a node is recomputed
but subsequently becomes unreachable from the top due to a new dependency
structure. We may reconsider this design choice in future, but for now
traversing the whole graph on each rebuild isn't a bottleneck even for
large-scale builds where graphs contain millions of nodes.

## Development status

Memo is still in active development and we welcome feedback from the OCaml
community on how to make it better. While the current implementation is tied to
Dune's lightweight concurrency library Fiber, the core functionality can be made
available as a functor over an arbitrary concurrency monad, making it useable
with Async and Lwt.

## Acknowledgements

We thank Rudi Horn, Rudi Grinberg, Emilio Jesús Gallego Arias, and other Dune
developers for their contributions to Memo and Dune. We are also grateful to
Armaël Guéneau for helping us integrate the incremental cycle detection library
in Memo.

# References

<!-- References to be generated by Pandoc. -->

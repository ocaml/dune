---
geometry: "left=2cm,right=2cm,top=2.4cm,bottom=2.1cm"
mainfont: Libertine
fontfamily: libertine
fontfamilyoptions:
  - mono=false
---

# Memo: the incremental computation library that powers Dune

Andrey Mokhov, Arseniy Alekseyev, Jeremie Dimino

*Jane Street, London, United Kingdom*

### Abstract

We present Memo, an incremental computation library that improves Dune's
performance on large-scale code bases. The requirements that come from the build
systems domain make Memo an interesting point in the design space of incremental
computation libraries. In particular, Memo needs to cope with concurrency,
dependency cycles, non-determinism, and scale to computation graphs comprising
millions of nodes.

## Introduction

The OCaml build system Dune [1] supports a continuous file-watching build mode,
where rebuilds are triggered automatically as the user is editing source files.
The simplest way to implement this functionality is to restart Dune on any file
change but that is too slow for large projects. To avoid re-scanning the
project's file tree, re-parsing all build specification files, and re-generating
all build rules from scratch every time, Dune uses an incremental computation
library called Memo. Below we briefly introduce the Memo's API.

Memo's `Build` monad is implemented on top of the *concurrency monad* `Fiber`.
Dune uses `Fiber` to speed up builds by executing external commands in parallel,
e.g., running multiple instances of `ocamlopt` to compile independent source
files. To inject a *fiber* into the `Build` monad, and to extract it back, one
can use the following pair of functions:

```ocaml
  val of_reproducible_fiber : 'a Fiber.t -> 'a Build.t
  val run : 'a Build.t -> 'a Fiber.t
```

More interestingly, functions in the `Build` monad can be memoized and cached
between different build runs:

```ocaml
  val create : ('i -> 'o Build.t) -> ('i, 'o) t (* A few arguments omitted for simplicity *)
  val exec : ('i, 'o) t -> 'i -> 'o Build.t
```

Here `t` is a memoization table that stores the input/output mapping computed in
the current build run. Outputs are stored alongside their *dependencies*, which
are automatically captured by Memo when memoized functions call one another.
Finally, Memo provides a way to *invalidate* a specific input/output pair, or a
*cell*, via the following API:

```ocaml
  val cell : ('i, 'o) t -> 'i -> ('i, 'o) Cell.t
  val invalidate : _ Cell.t -> unit
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

Firstly, to *capture dependencies*, Memo maintains a *call stack*, where stack
frames correspond to `exec` calls. The call stack is also useful for reporting
good error messages: when a user-supplied function raises an exception, we
extend it with the current Memo stack trace using human-readable annotations
provided via an optional argument of `create`.

<!-- Note that memoized functions can have diamond-shaped call graphs, and to avoid
reporting the same error multiple times, we need to deduplicate them. -->

* Why cycle detection and concurrency is hard combination (example)

* Interaction of build cancellation with error caching

* Interaction of concurrency and error-reporting (want errors to appear quickly
and in deterministic order -- impossible?)

* Push-based vs pull-based

Other relevant libraries:

* Incremental, Adapton: no support for concurrency, no cycle detection, push-based

* Tenacious: similar to Memo but is based on Async, has different early cutoff
  semantics, and relies on regular blocking cycle detection algorithm.

## Development status

* Currently under development, seeking feedback from the OCaml community

* Can be made available as a functor over an arbitrary concurrency monad

## Acknowledgements

We thank Rudi Horn, Rudi Grinberg, Emilio Jesús Gallego Arias, and other Dune
developers for their contributions to Memo and Dune. We are also grateful to
Armaël Guéneau for the help with the incremental cycle detection library that
we use in Memo.

# References

[1] Jane Street, 2018. *"Dune: A composable build system for OCaml"*.
    [https://dune.build/](https://dune.build/).

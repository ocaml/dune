---
geometry: "left=2cm,right=2cm,top=2.5cm,bottom=2.5cm"
mainfont: Libertine
fontfamily: libertine
fontfamilyoptions:
  - mono=false
---

# Memo: the incremental computation library that powers Dune

Andrey Mokhov, Arseniy Alekseyev, Jeremie Dimino

*Jane Street, London, United Kingdom*

### Abstract

We present Memo, an incremental computation library developed to support Dune's
continuous file-watching build mode. The requirements that come from the build
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
library called Memo.

... brief overview of the Memo's API ...

## Implementation notes

* Why cycle detection and concurrency is hard combination (example)

* Interaction of build cancellation with error caching

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

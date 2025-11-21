---
author: Etienne Millon
---

Your First Steps with Dune
====================

:::{warning}
This tutorial is a work in progress.
:::

In this tutorial, will start with a small Dune project, and extend it by using
common features:

- the {doc}`/reference/dune/executable`, {doc}`/reference/dune/library`, and
  {doc}`/reference/dune/test` {term}`stanzas <stanza>`;
- {doc}`cram </reference/cram>` tests;
- bindings to C code using {doc}`foreign stubs </reference/foreign-stubs>`;
- using a [ppx deriver](https://ocaml.org/docs/metaprogramming).

By doing so, you'll interact with `dune runtest` and `dune promote`, and will
use the most common {term}`stanzas <stanza>` in Dune files.

Start the tutorial with the {doc}`introduction`.

:::{toctree}
:hidden:
:maxdepth: 1
introduction
structure
development-cycle
interfacing-with-c
using-ppx
unit-tests
conclusion
:::

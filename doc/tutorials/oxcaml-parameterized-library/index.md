---
author: Etienne Marais
---

OxCaml Parameterized Library With Dune
======================================

:::{warning}
Parameterized libraries are a feature only supported by the OxCaml branch of
the OCaml compiler. You need to install this version of the compiler to use it
within Dune. This feature should currently be considered as not stable.
:::

This tutorial explains how to create a parameterized library in Dune.
Parameterized library in Dune are a concept similar to
{doc}`/virtual-libraries`. Parameterized libraries are the formalization within
the compiler of virtual libraries.A parameterized library is conceptually
equivalent to a functor. For more detailled explanation, see (TODO @maiste:
link to explanation).

By the end of the tutorial, you will know:
- how to create a library parameter,
- how to implement a library parameter,
- how to parameterize a library with a library parameter,
- how to instantiate a parameterized library with a a parameter implementation.

To get started make sure you have [oxcaml](https://oxcaml.org/) available in
your path and let's {doc}`setup` our project.

:::{toctree}
:hidden:
:maxdepth: 1
setup
parameter
implement
parameterized_by
instantiate
:::

---
author: Etienne Marais
---

OxCaml Parameterised Library With Dune
======================================

:::{warning}
Parameterised libraries are only supported by the OxCaml branch of the OCaml
compiler. You need to install this version of the compiler to use parameterised
libraries with Dune. This feature should be considered experimental.
:::

This tutorial explains how to create and use a parameterized library in Dune.
This feature enables the functorization of whole libraries, for example to do
dependency injection. Parameterized libraries provide a more flexible version
of dune's {doc}`/virtual-libraries`.

By the end of the tutorial, you will know:
- how to create a library parameter,
- how to implement a library parameter,
- how to parameterise a library with a library parameter,
- how to instantiate a parameterised library with a a parameter implementation.

## Getting started

Once you have checked that [OxCaml](https://oxcaml.org/) is available in your
path, you can enable the `oxcaml` extension in your `dune-project`:

:::{literalinclude} parameter/dune-project
:language: dune
:::

:::{important}
It is mandatory to add the `oxcaml` extension. It allows dune to use the
stanzas and fields specific to OxCaml. This extension is only available since
version `3.20` of dune.
:::

## Create a Library Parameter

In this section, we are going to learn how to write a library parameter.
Library parameters allow your library code to be generic with respect to the
implementation of the parameter, such that you can easily swap the
implementation for another. This is useful for dependency injection, testing,
platform-specific code, etc. The feature is most similar to OCaml `module type`
definitions, since a library parameter is just an OCaml mli interface.

First, we create a `param/` directory where we are going to host our parameter
definition.

```{code-block} shell
mkdir param
```

In `param/`, we create a new `dune` file using the
{doc}`/reference/dune/library_parameter` stanza to define a new library
parameter, which we have named `param`:

:::{literalinclude} parameter/param/dune
:language: dune
:::
::::

Still in `param/`, we also define the interface of the parameter in
`param.mli`:

:::{literalinclude} parameter/param/param.mli
:language: ocaml
:::
::::

This interface defines the types, functions and submodules, which
will later be provided by this parameter.

## Creating an Implementation

In this section, we will learn how to write a library that implements our
parameter interface.

As for the parameter, we are going to add a new directory `impl/` to host the
implementation code of our parameter:

```{code-block} shell
mkdir impl
```

An implementation of a parameter is just a library, with the `(implements param)`
annotation:

:::{literalinclude} parameter/impl/dune
:language: dune
:::

The implementation in `impl/impl.ml` must satisfy the parameter signature:

:::{literalinclude} parameter/impl/impl.ml
:language: ocaml
:::

## Creating a parameterised library

We'll define a library in the new folder `lib/` which is parameterised.
First create the new directory:

```{code-block} shell
mkdir lib
```

Then add a `lib/dune` file to declare the library, with the special
`(parameters param)` field to indicate it is generic with respect to the
parameter `param`:

:::{literalinclude} parameter/lib/dune
:language: dune
:::
::::

In the modules, we can then refer to `Param` even though we haven't
yet specified which concrete implementation to use. For example,
we can directly use `Param` functions:

:::{literalinclude} parameter/lib/lib.ml
:language: ocaml
:::
::::

## Instantiating a parameterised library

Other libraries can depend on our parameterised library, either because they
are themselves parameterised with the same parameter, or if they specify which
implementation of the parameter to use.  For executables, all the parameterised
libraries must be provided concrete arguments (otherwise we wouldn't be able to
run their code).

To conclude this tutorial, we define an executable target in the new folder `bin/`.
First create the directory:

```{code-block} shell
mkdir bin
```

And add a `bin/dune` file to define our executable:

:::{literalinclude} parameter/bin/dune
:language: dune
:::
::::

In the `libraries` dependencies, the syntax `(instantiate lib impl)` specifies
that we want to use the parameterised library `lib` with `impl` as the
implementation of its parameter `param`.

The code for our binary simply calls the library code, using the name `Lib`:

:::{literalinclude} parameter/bin/bin.ml
:language: ocaml
:::
::::

## Conclusion

In this tutorial, we have learned how to define a parameter, its
implementation, a parameterised library and how to instantiate them for an
executable target.

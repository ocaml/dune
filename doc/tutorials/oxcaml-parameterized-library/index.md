---
author: Etienne Marais, Arthur Wendling, Sudha Parimala, Shon Feder
---

OxCaml Parameterised Libraries with Dune
======================================

::: {warning}
- This feature is experimental.
- Parameterised libraries are only supported by the OxCaml branch of the OCaml
compiler. You need to [install the OxCaml compiler][get-oxcaml] to use
parameterised libraries with Dune.
:::

[get-oxcaml]: https://oxcaml.org/get-oxcaml/

This tutorial explains how to create and use a parameterized library in Dune.
*Parameterised libraries* specify some of their dependencies as *library
parameters*, without committing a concrete implementations. This is useful for
build-time dependency injection and for generalizing over platform-specific
implementations. It provides a more flexible alternative to dune's
{doc}`/virtual-libraries`.

By the end of the tutorial, you will know:

- how to declare a library parameter,
- how to implement a library parameter,
- how to parameterise a library on a library parameter,
- how to instantiate a parameterised library by supplying an implementation of
  its library parameters.

## Getting Started

Once you have checked that [OxCaml](https://oxcaml.org/) is installed and its
`ocamlc` is available on your path, you can enable Dune's `oxcaml` extension in
your `dune-project`:

:::{literalinclude} parameter/dune-project
:language: dune
:::

:::{important}
Enabling the OxCaml extension by adding the `(use oxcaml 0.1)` stanza allows
dune to use the stanzas and fields specific to OxCaml. This extension is only
available since dune `3.20`.
:::

## Declare a Library Parameter

Library parameters allow libraries to be generic over libraries they depend on.
This is useful for dependency injection, testing, platform-specific code, etc.

:::{note}
The interface of a library parameter is specified using an OCaml `.mli` file and
declared via the `library_parameter` dune stanza.  The following analogy holds:

|                 | module parameters                 | library parameters                        |
|-----------------|-----------------------------------|-------------------------------------------|
| *declaration*   | `module type Intf = <sig>`        | `(library_parameter (name intf))`  stanza |
| *specification* | a module signature: `sig ... end` | an interface file: e.g.,  `intf.mli`      |
:::


First, we create an (arbitrarily named) `param/` directory where we are going to
write our parameter definition.

```{code-block} shell
mkdir param
```

We create a `param/dune` file containing a
{doc}`/reference/dune/library_parameter` stanza that declares a new library
parameter named `param`:

:::{literalinclude} parameter/param/dune
:language: dune
:::

To complete the declaration, we must provide an `.mli` file matching the given
`name`, which will specify the interface of the library parameter. We create
this in `param/param.mli`:

:::{literalinclude} parameter/param/param.mli
:language: ocaml
:::

This interface specifies the types, functions, and modules which must be
provided by a library for it to implement the declared parameter.

## Implement a Library Parameter

:::{note}
The implementation of a library parameter must satisfy the signature of the
parameter it claims to implement. The following analogy holds:

|                        | module implementation                 | library implementation                                                                   |
|------------------------|---------------------------------------|------------------------------------------------------------------------------------------|
| *ascribed declaration* | `module Impl : Intf`                  | a library `stanza` with an `implements` field: `(library (name impl) (implements intf))` |
| *implementation*       | a module expression: `struct ... end` | a set of `.ml` files                                                                     |
:::

We create an (arbitrarily named) directory `impl/`  where we are going to
write an implementation of our library parameter:

```{code-block} shell
mkdir impl
```

An implementation of a declared library parameter named `param` is a normal dune
library with the `(implements param)` annotation. We declare this `impl/dune`:

:::{literalinclude} parameter/impl/dune
:language: dune
:::

For the sake of simplicity, we give the example of a single-module library,
defined in `impl/impl.ml`. This must satisfy the parameter interface ascribed by
the `implements` field:

:::{literalinclude} parameter/impl/impl.ml
:language: ocaml
:::

## Define a Parameterised Library

:::{note}
A parameterised library must specify the parameters it requires. The following
analogy holds:

|               | parameterised module (a "functor") | parameterised library                           |
|---------------|------------------------------------|-------------------------------------------------|
| *declaration* | `module M (P : Intf)`              | a `(library (name m) (parameters intf))` stanza |
:::

We create an (arbitrarily named) directory `lib/`  where we are going to write a
library that is parameterised on our declared library parameter.

```{code-block} shell
mkdir lib
```

We add a `lib/dune` file to declare the library, with the `(parameters param)`
field, indicating that the library is generic over the library parameter `param`:

:::{literalinclude} parameter/lib/dune
:language: dune
:::


Within the modules comprising a parameterised library, we can then refer library
parameters without having to decide on a concrete implementation of the
libraries up front. In our example, we can directly use the functions from the
declared `Param` in our library `lib/lib.ml`:

:::{literalinclude} parameter/lib/lib.ml
:language: ocaml
:::

## Instantiate a Parameterised Library

:::{note}
To instantiate a parameterised library, we must supply library arguments that
implement all specified parameters. The following analogy holds:

|                 | functor application | library instantiation                                                      |
|-----------------|---------------------|----------------------------------------------------------------------------|
| *instantiation* | `M (P)`             | a `(instantiate m p)` expression in {doc}`/reference/library-dependencies` |
:::

A *library* can depend on parameterised Libraries if it is, either,
parameterised with the same parameters as its dependencies, or, it
**instantiates** its parameterised dependencies. An *executable*, however, must
instantiate any parameterised libraries, otherwise we wouldn't be able to
compile a complete program to execute.

To conclude this tutorial, we define an executable target in the new folder
`bin/`, that depends on an instantiation of our parameterised library.

We create an (arbitrarily named) `bin` directory:

```{code-block} shell
mkdir bin
```

Then we add a `bin/dune` file defining our executable:

:::{literalinclude} parameter/bin/dune
:language: dune
:::

In the `libraries` field, the syntax `(instantiate lib impl)` specifies that we
want to use the concrete library resulting from applying the parameterised
library `lib` to the library argument `impl`. Note that this application is only
possible because we defined the `impl` library as an implementation of the
library parameter, `param`.

The code for our binary simply calls the library code, using the library's name, `Lib`:

:::{literalinclude} parameter/bin/bin.ml
:language: ocaml
:::

## Conclusion

In this tutorial, we have learned how to declare a library parameter, define its
implementation, define a parameterised library, and finally to use the latter by
instantiating it for an executable target.

# Setting Up Package Management With Dune

The idea of package management with Dune has been as unobtrusive as
possible. Thus most projects can easily be built with just the minimum of
changes.

In this tutorial we will create a simple project to use the integrated package
management feature for the very first time.

## Declare Dependencies

The best way to work with the package management is to declare your
dependencies in the `dune-project` file.

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} setup/dune-project
:language: dune
:emphasize-lines: 6-7
:::

We define a project called `test` and declare that to build it we need an OCaml
compiler that is at least version 4.14.

This is exactly the same information that is used to generate opam files using
the `generate_opam_files` stanza as described in
{doc}`/howto/opam-file-generation`.

::::

::::{dropdown} `test.ml`
:icon: file-code

:::{literalinclude} setup/test.ml
:language: ocaml
:::

To show that the build works, this simple program will be built and executed.

::::


::::{dropdown} `dune`
:icon: file-code

:::{literalinclude} setup/dune
:language: dune
:::

To declare our module an executable we need a little bit of configuration, so we
just define the module as an executable.

::::

After our project skeleton is set up, we can proceed to the next step.

## Locking Dependencies

After declaring the dependencies, you will need to tell Dune which package
versions to use for your project. This is done by creating a lock directory.
This is easily done with a new Dune command:

```
$ dune pkg lock
Solution for .dune-solution-cache:
- ocaml.5.2.0
- ocaml-base-compiler.5.2.0
- ocaml-config.3
```

This will update all the required opam repositories, use the newest version of
each and try to find a set of packages and versions that satisfy the
constraints that your project dependencies declare.

:::{note}
The versions that get locked might be different from this tutorial, as we only
specified the lower bound of `ocaml`; barring any additional configuration, Dune
will pick the newest possible version for each dependency.
:::

## Build Project

To build the project, you can just use the regular Dune commands.

```sh
dune build
```

This will download, build, and install all your locked dependencies and then use
those to build your project. This means that the first time building it will take
longer than usual, as the dependencies need to be built first. Subsequent
builds where all dependencies have been built before will be just as fast as
before.

We can show that the package has been built successfully and works as expected:

```
$ dune exec ./test.exe
Hello, OCaml, Rust!
```

:::{note}
If you want to only build and fetch the project dependencies, you can use
the `@pkg-install` alias like so

```shell
$ dune build @pkg-install
```

See {doc}`/reference/aliases/pkg-install` for more information.
:::

## Conclusion

In this section we learned how to set up a Dune project that picks a compiler
and installs it without the need for any additional tooling.

In the next section {doc}`dependencies` we will look on how to add third party
dependencies.

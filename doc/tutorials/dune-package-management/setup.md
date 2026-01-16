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

## Enable package management

So far we have done everything as in a regular Dune project. However, now we
need to tell Dune that we want to use the package management feature. To do so
we have to add a new stanza to our `dune-workspace` file, creating it if it
doesn't exist.

::::{dropdown} `dune-workspace`
:icon: file-code

:::{literalinclude} setup/dune-workspace
:language: dune
:emphasize-lines: 2
:::

This new stanza will tell Dune to determine the packages that your project
depends on, find the right versions, download, and build them on the next build.

::::

## Build Project

To build the project, you can just use the regular Dune commands.

```sh
dune build
```

Since this is the first time we have run the build system after enabling package
management a number of things will happen:

 1. It will download and update all required opam repositories to determine
    which packages are available.
 2. It will attempt to find a package solution that satisfies all dependency
    constraints.
 3. It will download the sources of the dependencies.
 4. It will build the dependencies in sandbox locations.
 5. It will install the dependencies in the build folder.
 6. It will build the project using the dependencies that it has installed.

This means that building the first time will take longer than usual, as the
dependencies need to be built, possibly including the OCaml compiler.
Subsequent builds where all dependencies have already been built will
be significantly faster.

We can show that the package has been built successfully and works as expected:

```
$ dune exec ./test.exe
Hello, OCaml, Rust!
```

:::{note}
If you don't want to build your project, instead stopping at step 5, you can use
the `@pkg-install` alias like so

```shell
$ dune build @pkg-install
```

This functionality can be useful to cache the installation of dependencies,
somewhat similar to `opam switch create` followed by `opam install`.

See {doc}`/reference/aliases/pkg-install` for more information.
:::

## Conclusion

In this section we learned how to set up a Dune project that picks a compiler
and installs it without the need for any additional tooling.

In the next section {doc}`dependencies` we will look on how to add third party
dependencies.

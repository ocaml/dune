# Managing Dependencies

The OCaml ecosystem has a wealth of third-party packages that are available for
use. In this section we will look into how to use them with Dune.

## Adding Dependencies

Much like in regular projects, to add a library we need to add a dependency to
it. For simplicity we will use the popular `fmt` library as an example, but any
package from the [package repository](https://ocaml.org/packages) can be used.

First we update the `dune-project` file to add a dependency on the opam package.

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} dependencies/dune-project
:language: dune
:emphasize-lines: 8
:::

::::

After this change to our project dependencies, we need to relock dependencies
to update our lock directory with the new packages.

```
$ dune pkg lock
Solution for .dune-solution-cache:
- base-unix.base
- fmt.0.9.0
- ocaml.5.2.0
- ocaml-base-compiler.5.2.0
- ocaml-config.3
- ocamlbuild.0.15.0+dune
- ocamlfind.1.9.6+dune
- topkg.1.0.7
```

You can see a lot of new dependencies, among these `fmt`.

:::{note}
The list of packages being output includes all dependencies of your project,
including transitive dependencies.
:::

This will take care of installing the dependencies, but we still need to add it to
our build as a library as usual:

::::{dropdown} `dune`
:icon: file-code

:::{literalinclude} dependencies/dune
:language: dune
:emphasize-lines: 3
:::

Adding a library dependency to our `dune` file via the `libraries` stanza.

::::

This will allow us to use the `Fmt` module in our OCaml code.

::::{dropdown} `test.ml`
:icon: file-code

:::{literalinclude} dependencies/test.ml
:language: ocaml
:emphasize-lines: 4
:::

We update the code to define an `Fmt.t` pretty-printer for the list of strings
and then use it to print the value.

::::

To build it we just call `build` again.

```
$ dune build
```

which will download and install the new dependencies and build our project as
before.

As we see, the code works and uses `fmt` to do the pretty-printing:

```
$ dune exec ./test.exe
Hello, OCaml, Rust!
```

### Dependency Constraints

Packages are often only compatible with some versions of dependencies. To
specify a version range, use the regular Dune dependency syntax
used for opam dependencies in the `dune-project` file.

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} dependencies/constraints
:language: dune
:emphasize-lines: 7-8
:::

::::

This ensures the `fmt` package to install will be compatible with
our request. These constraints will be taken into account the next time the
package is locked:

```
$ dune pkg lock
Solution for .dune-solution-cache:
- base-unix.base
- fmt.0.9.0
- ocaml.5.2.0
- ocaml-base-compiler.5.2.0
- ocaml-config.3
- ocamlbuild.0.15.0+dune
- ocamlfind.1.9.6+dune
- topkg.1.0.7
```

The version of `fmt` picked is indeed between `0.6` and `1.0`.

## Removing Dependencies

Given all dependencies are defined in the `dune-project` file, removing a
dependency means to remove the dependency from the `depends` field of your
`dune-project` and relocking the project.

The new lock directory will not depend on the package anymore, and in future
builds, the package will not be accessible as `library` anymore.

:::{note}
The removed dependency might still be part of the lock directory if some other
dependency of your project depends on it.
:::

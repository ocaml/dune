# Managing Dependencies

The OCaml ecosystem has a wealth of third-party packages that are available for
use. In this section we will look into how to use them with Dune.

## Adding Dependencies

Much like in regular projects, to add a library we need to add a dependency to
it. For simplicity we will use the popular `fmt` library as an example, but any
package from the [package repository](https://ocaml.org/packages) can be used.

To do so we update the `dune-project` file to add a dependency on the opam
package.

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} dependencies/dune-project
:language: dune
:emphasize-lines: 8
:::

Here we define the OPAM packages that we want to use, along with the version
constraints these dependencies should adhere to.

::::

::::{dropdown} `dune-workspace`
:icon: file-code

:::{literalinclude} dependencies/dune-workspace
:language: dune
:emphasize-lines: 2
:::

In this file we direct Dune to enable package management in the current
workspace. The `pkg` stanza configures Dune to manage the declared dependencies
automatically.

::::

This configuration will take care of installing the dependencies, but we still
need to add it to our build as a library as usual:

::::{dropdown} `dune`
:icon: file-code

:::{literalinclude} dependencies/dune
:language: dune
:emphasize-lines: 3
:::

Adding a library dependency to our `dune` file via the `libraries` stanza. This
is unchanged from the usual Dune workflow.

::::

This change will allow us to use the `Fmt` module in our OCaml code.

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

Dune will notice that the project depends on new packages. Thus it will re-run
the internal dependency solver to find a solution for the set of packages to
use. It will then use this new solution to download, build and install these
dependencies automatically.

We can check the build log in `_build/log` and see the packages that the Dune
solver has selected:

```
...
# Dependency solution for
# _build/.sandbox/<sandbox-hash>/_private/default/.lock/dune.lock:
# - base-unix.base
# - fmt.0.11.0
# - ocaml.5.4.0
# - ocaml-base-compiler.5.4.0
# - ocaml-compiler.5.4.0
# - ocaml-config.3
# - ocamlbuild.0.16.1+dune
# - ocamlfind.1.9.8+dune
# - topkg.1.1.1
...
```

:::{note}
The list of packages being output includes all dependencies of your project,
including transitive dependencies.
:::

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

This change ensures the `fmt` package to install will be compatible with our
request. These constraints will be taken into account the next time the build
system is ran.

```sh
dune build
```

Checking `_build/log` again reveals that our change was taken into account:

```
...
# Dependency solution for
# _build/.sandbox/<sandbox-hash>/_private/default/.lock/dune.lock:
# - base-unix.base
# - fmt.0.9.0
# - ocaml.5.4.0
# - ocaml-base-compiler.5.4.0
# - ocaml-compiler.5.4.0
# - ocaml-config.3
# - ocamlbuild.0.16.1+dune
# - ocamlfind.1.9.8+dune
# - topkg.1.1.1
...
```

## Removing Dependencies

Given all dependencies are defined in the `dune-project` file, removing a
dependency just means to remove the dependency from the `depends` field of your
`dune-project`.

From then on the project will not depend on the package anymore, and in future
builds the package will not be accessible as `library` anymore.

:::{note}
The removed dependency might still be accessible if some other dependency of
your project depends on it, thus if it is a transitive dependency.
:::


## External Dependencies

Many packages also declare external system dependencies ("depexts") that must
be installed in order to compile or use the package. This information is stored
in the package's `.opam` file.

To view these external dependencies, use `dune show depexts`. The output can
then be used to install the required system packages via your system package
manager.

For example, the [`postgresql`](https://ocaml.org/p/postgresql/5.3.2) library
shows the following depexts on an Ubuntu system:

```sh
$ dune show depexts
libpq-dev
pkg-config
```

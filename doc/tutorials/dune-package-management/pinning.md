# Pinning Projects

When Dune is looking up packages to lock, it uses the (pre)configured OCaml
package repositories. However it is also possible to manually specify the
sources of packages; for example, if the package is not released in a package
repository. This is called "pinning".

## Installing Packages From a Pin

To pin a package, a new `pin` has to be declared in the `dune-project` file.

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} pinning/dune-project
:language: dune
:emphasize-lines: 4-6,12
:::

This will create a pin on the `fmt` package and use the specified Git
repository URL to retrieve the sources. For more information refer to {doc}`the
pin stanza reference </reference/dune-project/pin>`.

Don't forget to remove the version constraints from `fmt` in the list of
dependencies.

::::

The next time the project is built, Dune will use this repository instead of
the information from the selected package repositories. Thus the `fmt` package
will be built from the source in the specified Git repository rather than from
the source tarball released in the `opam-repository`.

```sh
dune build
```

will find a new solution, quoting `_build/log`:

```
...

# Dependency solution for
# _build/.sandbox/<sandbox-hash>/_private/default/.lock/dune.lock:
# - base-unix.base
# - fmt.dev
# - ocaml.5.4.0
# - ocaml-base-compiler.5.4.0
# - ocaml-compiler.5.4.0
# - ocaml-config.3
# - ocamlbuild.0.16.1+dune
# - ocamlfind.1.9.8+dune
# - topkg.1.1.1
...
```

Unlike previously, the version of the `fmt` library that is picked is `dev`, to
signify a development version.


```
$ dune exec ./test.exe
Hello, OCaml, Rust!
```

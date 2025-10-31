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

The next time the package is locked, Dune will use this repository instead of
the information from the selected package repositories.

```
$ dune pkg lock
Solution for .dune-solution-cache:
- base-unix.base
- fmt.dev
- ocaml.5.0.0
- ocaml-base-compiler.5.0.0
- ocaml-config.3
- ocamlbuild.0.15.0+dune
- ocamlfind.1.9.6+dune
- topkg.1.0.7
```

Unlike previously, the version of the `fmt` library that is picked is `dev`, to
signify a development version.

The next time the project is built, the `fmt` package will be built from the
source in the specified Git repository rather than from the source tarball
released in the `opam-repository`.

```
$ dune exec ./test.exe
Hello, OCaml, Rust!
```

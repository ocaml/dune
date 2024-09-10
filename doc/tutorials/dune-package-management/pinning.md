# Pinning Projects

When Dune is looking up packages to lock, it uses the (pre)configured OCaml
package repositories. However it is also possible to manually specify the
sources for packages; for example, if the package is not released in a package
repository. This is called "pinning."

## Installing Packages From a Pin

To pin a package, a new `pin` has to be declared in the `dune-project` file.

::::{dropdown} `dune-project`
:icon: file-code

:::{literalinclude} pinning/dune-project
:language: dune
:emphasize-lines: 4-6
:::

This will create a pin on the `fmt` package and use the specified Git repository
URL to retrieve the sources.

::::

The next time the package is locked, Dune will use this repository instead of
the information from the selected package repositories.

```sh
$ dune pkg lock
Solution for dune.lock:
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

The next build will check out the sources from that repository instead of
downloading the release tarball:

```sh
$ dune exec ./test.exe
Hello, OCaml, Rust!
```

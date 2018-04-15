# Hacking on Dune

This section is for people who want to work on Jbuilder itself.

## Bootstrap

In order to build itself, Jbuilder uses an OCaml script
([bootstrap.ml](bootstrap.ml)) that dumps most of the sources of Jbuilder into a
single `boot.ml` file. This file is built using `ocamlopt` or `ocamlc`
and used to build everything else.

Note that we don't include all of the sources in boot.ml. We skip a
few parts to speed up the build. In particular:
- vendored libraries are replaced by simpler implementations taken
  from `vendor/boot`
- a few files in `src` have an alternative version. These alternatives
  versions are named `XXX.boot.EXT`. For instance: `glob_lexer.boot.ml`

## OCaml compatibility test

Install opam switches for all the entries in the
[jbuild-workspace.dev](jbuild-workspace.dev) file and run:

```sh
$ make all-supported-ocaml-versions
```

## Repository organization

- `vendor/` contains dependencies of Jbuilder, that have been vendored
- `plugin/` contains the API given to `jbuild` files that are OCaml
  scripts
- `src/` contains the core of `Jbuilder`, as a library so that it can
  be used to implement the Jenga bridge later
- `bin/` contains the command line interface
- `doc/` contains the manual and rules to generate the manual pages

## Design

Jbuilder was initially designed to sort out the public release of Jane
Street packages which became incredibly complicated over time. It is
still successfully used for this purpose.

One necessary feature to achieve this is the ability to precisely
report the external dependencies necessary to build a given set of
targets without running any command, just by looking at the source
tree. This is used to automatically generate the `<package>.opam`
files for all Jane Street packages.

To implement this, the build rules are described using a build arrow,
which is defined in [src/build.mli](src/build.mli). In the end it makes the
development of the internal rules of Jbuilder very composable and
quite pleasant.

To deal with process multiplexing, Jbuilder uses a simplified
Lwt/Async-like monad, implemented in [src/future.mli](src/future.mli).

## Code flow

- [src/jbuild.mli](src/jbuild.mli) contains the internal representation
  of `jbuild` files and the parsing code
- [src/jbuild_load.mli](src/jbuild_load.mli) contains the code to scan
  a source tree and build the internal database by reading
  the `jbuild` files
- [src/gen_rules.mli](src/gen_rules.mli) contains all the build rules
  of Jbuilder
- [src/build_system.mli](src/build_system.mli) contains a trivial
  implementation of a Build system. This is what Jenga will provide
  when implementing the bridge

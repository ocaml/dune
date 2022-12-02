Dune - A Composable Build System
================================

Dune is a build system designed for OCaml/Reason projects only. It focuses on
providing the user with a consistent experience and takes care of most low-level
details of OCaml compilations. It's merely necessary to provide a description of
your project, and Dune will do the rest.

It implements a scheme that's inspired from the one used inside Jane Street and
adapted to the open source world. It has matured over a long time and is used
daily by hundreds of developers, meaning it's highly tested and productive.

Dune comes with a [manual][manual]. If you want to get started without reading
too much, look at the [quick start guide][quick-start] or watch [this
introduction video][video].

The [example][example] directory contains examples of projects using dune.

[![Main workflow][workflow-badge]][workflow]
[![AppVeyor status][appveyor-img]][appveyor]
[![Release][release-img]][release]

[manual]:         https://dune.readthedocs.io/en/latest/
[quick-start]:    https://dune.readthedocs.io/en/latest/quick-start.html
[example]:        https://github.com/ocaml/dune/tree/master/example
[merlin]:         https://github.com/ocaml/merlin
[opam]:           https://opam.ocaml.org
[issues]:         https://github.com/ocaml/dune/issues
[dune-release]:   https://github.com/ocamllabs/dune-release
[video]:          https://youtu.be/BNZhmMAJarw

Overview
--------

Dune reads project metadata from `dune` files, which are either static files in
a simple S-expression syntax or OCaml scripts. It uses this information to setup
build rules, generate configuration files for development tools such as
[Merlin][merlin], handle installation, etc.

Dune itself is fast, has very low overhead, and supports parallel builds on all
platforms. It has no system dependencies. OCaml is all you need to build Dune
and packages using Dune. You don't need `make` or `bash`, as long as the
packages themselves don't use `bash` explicitly.

In particular, one can install OCaml on Windows with a binary installer and then
use only the Windows Console to build Dune and packages using Dune.

Strengths
---------

### Composable

Take *n* repositories that use Dune and arrange them in any way on the file
system. The result is still a single repository that Dune knows how to build at
once.

This make simultaneous development on multiple packages trivial.

### Gracefully Handles Multi-Package Repositories

Dune knows how to handle repositories containing several packages. When building
via [Opam][opam], it is able to correctly use libraries that were previously
installed, even if they are already present in the source tree.

The magic invocation is:

```sh
$ dune build --only-packages <package-name> @install
```

### Building Against Several Configurations at Once

Dune can build a given source code repository against several configurations
simultaneously. This helps maintaining packages across several versions of
OCaml, as you can test them all at once without hassle.

In particular, this makes it easy to handle
[cross-compilation](https://dune.readthedocs.io/en/latest/cross-compilation.html).

This feature requires [Opam][opam].

Requirements
------------

Dune requires OCaml version 4.08.0 to build itself and can build OCaml projects
using OCaml 4.02.3 or greater.

Installation
------------

We recommended installing Dune via the [Opam package manager][opam]:

```sh
$ opam install dune
```

If you are new to Opam, make sure to run `eval $(opam config env)` to make
`dune` available in your `PATH`. The `dune` binary is self-contained and
relocatable, so you can safely copy it somewhere else to make it permanently
available.

You can also build it manually with:

```sh
$ make release
$ make install
```

If you do not have `make`, you can do the following:

```sh
$ ocaml boot/bootstrap.ml
$ ./dune.exe build -p dune --profile dune-bootstrap
$ ./dune.exe install dune
```

The first command builds the `dune.exe` binary. The second builds the additional
files installed by Dune, such as the *man* pages, and the last simply installs
all of that on the system.

**Please note**: unless you ran the optional `./configure` script, you can
simply copy `dune.exe` anywhere and it will just work. `dune` is fully
relocatable and discovers its environment at runtime rather than hard-coding it
at compilation time.

Support
-------

If you have questions about Dune, you can send an email to
ocaml-core@googlegroups.com or [open a ticket on GitHub][issues].


Migration from Jbuilder
-----------------------

Dune was formerly known as Jbuilder. Migration from Jbuilder to Dune is
described in the [manual](http://dune.readthedocs.io/en/latest/migration.html).

Status
------

Dune is fairly stable and used by the majority of packages on Opam. Note that
Dune retains backward compatibility with Jbuilder, and in particular, existing
Jbuilder projects will continue to be buildable with Dune.

[appveyor]:       https://ci.appveyor.com/project/diml/dune/branch/master
[appveyor-img]:   https://ci.appveyor.com/api/projects/status/rsxayce22e8f2jkp?svg=true
[workflow-badge]: https://github.com/ocaml/dune/actions/workflows/workflow.yml/badge.svg
[workflow]:       https://github.com/ocaml/dune/actions/workflows/workflow.yml
[release]:        https://github.com/ocaml/dune/releases
[release-img]:    https://img.shields.io/github/release/ocaml/dune.svg

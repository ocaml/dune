![Dune][logo]

# A Composable Build System for OCaml

[![Main Workflow][workflow-badge]][workflow]
[![Release][release-badge]][release]
[![Coverage][coverage-badge]][coverage]
[![License][license-badge]][license]
[![Contributors][contributors-badge]][contributors]

[logo]: doc/assets/imgs/dune_logo_459x116.png
[workflow]: https://github.com/ocaml/dune/actions/workflows/workflow.yml
[workflow-badge]: https://img.shields.io/github/actions/workflow/status/ocaml/dune/workflow.yml?label=CI&logo=github
[release]: https://github.com/ocaml/dune/releases/latest
[release-badge]: https://img.shields.io/github/v/release/ocaml/dune?label=release
[coverage]: https://coveralls.io/github/ocaml/dune
[coverage-badge]: https://img.shields.io/coveralls/github/ocaml/dune?logo=coveralls
[license]: https://github.com/ocaml/dune/blob/main/LICENSE.md
[license-badge]: https://img.shields.io/github/license/ocaml/dune
[contributors]: https://github.com/ocaml/dune/graphs/contributors
[contributors-badge]: https://img.shields.io/github/contributors-anon/ocaml/dune

Dune is a build system for OCaml. It provides a consistent experience and takes
care of the low-level details of OCaml compilation. You need only to provide a
description of your project, and Dune will do the rest.

Dune implements a scheme that's inspired from the one used inside Jane Street
and adapted to the open source world. It has matured over a long time and is
used daily by hundreds of developers, meaning it's highly tested and productive.

Dune comes with a [manual][manual]. If you want to get started without reading
too much, look at the [quick start guide][quick-start] or watch [this
introduction video][video].

The [example][example] directory contains examples of projects using Dune.

[manual]: https://dune.readthedocs.io/en/latest/
[quick-start]: https://dune.readthedocs.io/en/latest/quick-start.html
[example]: https://github.com/ocaml/dune/tree/main/example
[merlin]: https://github.com/ocaml/merlin
[opam]: https://opam.ocaml.org
[issues]: https://github.com/ocaml/dune/issues
[discussions]: https://github.com/ocaml/dune/discussions
[dune-release]: https://github.com/ocamllabs/dune-release
[video]: https://youtu.be/BNZhmMAJarw

# How does it work?

Dune reads project metadata from `dune` files, which are static files with a
simple S-expression syntax. It uses this information to setup build rules,
generate configuration files for development tools such as [Merlin][merlin],
handle installation, etc.

Dune itself is fast, has very little overhead, and supports parallel builds on
all platforms. It has no system dependencies. OCaml is all you need to build
Dune and packages using Dune.

In particular, one can install OCaml on Windows with a binary installer and then
use only the Windows Console to build Dune and packages using Dune.

# Strengths

## Composable

Dune is composable, meaning that multiple Dune projects can be arranged
together, leading to a single build that Dune knows how to execute. This allows
for monorepos of projects.

Dune makes simultaneous development on multiple packages a trivial task.

## Gracefully Handles Multi-Package Repositories

Dune knows how to handle repositories containing several packages. When building
via [opam][opam], it is able to correctly use libraries that were previously
installed, even if they are already present in the source tree.

The magic invocation is:

```console
$ dune build --only-packages <package-name> @install
```
## Build Against Several Configurations at Once

Dune can build a given source code repository against several configurations
simultaneously. This helps maintaining packages across several versions of
OCaml, as you can test them all at once without hassle.

In particular, this makes it easy to handle
[cross-compilation][cross-compilation]. This feature requires [opam][opam].

[cross-compilation]: https://dune.readthedocs.io/en/latest/cross-compilation.html

# Installation

## Requirements

Dune requires OCaml version 4.08.0 to build itself and can build OCaml projects
using OCaml 4.02.3 or greater.

## Installation

We recommended installing Dune via the [opam package manager][opam]:

```console
$ opam install dune
```

If you are new to opam, make sure to run `eval $(opam config env)` to make
`dune` available in your `PATH`. The `dune` binary is self-contained and
relocatable, so you can safely copy it somewhere else to make it permanently
available.

You can also build it manually with:

```console
$ make release
$ make install
```

If you do not have `make`, you can do the following:

```console
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

# Support


[![Issues][issues-badge]][issues]
[![Discussions][discussions-badge]][discussions]
[![Discuss OCaml][discuss-ocaml-badge]][discuss-ocaml]
[![Discord][discord-badge]][discord]

If you have questions or issues about Dune, you can ask in [our GitHub
discussions page][discussions] or [open a ticket on GitHub][issues].

[discussions]: https://github.com/ocaml/dune/discussions
[discussions-badge]: https://img.shields.io/github/discussions/ocaml/dune?logo=github
[issues]: https://github.com/ocaml/dune/issues
[issues-badge]: https://img.shields.io/github/issues/ocaml/dune?logo=github
[discuss-ocaml]: https://discuss.ocaml.org
[discuss-ocaml-badge]: https://img.shields.io/discourse/topics?server=https%3A%2F%2Fdiscuss.ocaml.org%2F
[discord]: https://discord.com/invite/cCYQbqN
[discord-badge]: https://img.shields.io/discord/436568060288172042?logo=discord

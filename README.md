Jbuilder - A composable build system
====================================

Jbuilder is a build system designed for OCaml/Reason projects only. It
focuses on providing the user with a consistent experience and takes
care of most of the low-level details of OCaml compilation. All you
have to do is provide a description of your project and Jbuilder will
do the rest.

The scheme it implements is inspired from the one used inside Jane
Street and adapted to the open source world. It has matured over a
long time and is used daily by hundreds of developers, which means
that it is highly tested and productive.

Jbuilder comes with a [manual][manual]. If you want to get started
without reading too much, you can look at the [quick start
guide][quick-start] or watch [this introduction video][video].

The [example][example] directory contains examples of projects using
jbuilder.

[![Travis status][travis-img]][travis] [![AppVeyor status][appveyor-img]][appveyor]

[manual]:         https://jbuilder.readthedocs.io/en/latest/
[quick-start]:    https://jbuilder.readthedocs.io/en/latest/quick-start.html
[example]:        https://github.com/janestreet/jbuilder/tree/master/example
[travis]:         https://travis-ci.org/janestreet/jbuilder
[travis-img]:     https://travis-ci.org/janestreet/jbuilder.svg?branch=master
[appveyor]:       https://ci.appveyor.com/project/diml/jbuilder/branch/master
[appveyor-img]:   https://ci.appveyor.com/api/projects/status/bn3kcxx648jt6dyt?svg=true
[merlin]:         https://github.com/ocaml/merlin
[opam]:           https://opam.ocaml.org
[jenga]:          https://github.com/janestreet/jenga
[issues]:         https://github.com/janestreet/jbuilder/issues
[topkg-jbuilder]: https://github.com/diml/topkg-jbuilder
[video]:          https://youtu.be/BNZhmMAJarw

Overview
--------

Jbuilder reads project metadata from `jbuild` files, which are either
static files in a simple S-expression syntax or OCaml scripts. It uses
this information to setup build rules, generate configuration files
for development tools such as [merlin][merlin], handle installation,
etc...

Jbuilder itself is fast, has very low overhead and supports parallel
builds on all platforms. It has no system dependencies: all you need
to build jbuilder and packages using jbuilder is OCaml. You don't need
`make` or `bash` as long as the packages themselves don't use `bash`
explicitly.

Especially, one can install OCaml on Windows with a binary installer
and then use only the Windows Console to build Jbuilder and packages
using Jbuilder.

Strengths
---------

### Composable

Take n repositories that use Jbuilder, arrange them in any way on the
file system and the result is still a single repository that Jbuilder
knows how to build at once.

This make simultaneous development on multiple packages trivial.

### Gracefully handles multi-package repositories

Jbuilder knows how to handle repositories containing several
packages. When building via [opam][opam], it is able to correctly use
libraries that were previously installed even if they are already
present in the source tree.

The magic invocation is:

```sh
$ jbuilder build --only-packages <package-name> @install
```

### Building against several configurations at once

Jbuilder is able to build a given source code repository against
several configurations simultaneously. This helps maintaining packages
across several versions of OCaml as you can tests them all at once
without hassle.

This feature should make cross-compilation easy, see details in the
[roadmap](ROADMAP.md).

This feature requires [opam][opam].

### Jenga bridge

[Jenga][jenga] is another build system for OCaml that has more
advanced features such as polling or much better editor
integration. Jenga is more powerful and more complex and as a result
has many more dependencies.  It is planned to implement a small bridge
between the two so that a Jbuilder project can build with Jenga using
this bridge.

Requirements
------------

Jbuilder requires OCaml version 4.02.3 or greater.

installation
------------

The recommended way to install jbuilder is via the
[opam package manager][opam]:

```sh
$ opam install jbuilder
```

You can also build it manually with:

```sh
$ make release
$ make install
```

Note however that `make install` requires the `opam-installer`
tool. Running simply `make` will build jbuilder using the development
settings.

If you do not have `make`, you can do the following:

```sh
$ ocaml bootstrap.ml
$ ./boot.exe
$ ./_build/default/bin/main.exe install
```

Support
-------

If you have questions about jbuilder, you can send an email to
ocaml-core@googlegroups.com or [open a ticket on github][issues].

Status
------

Jbuilder is now in beta testing stage. Once a bit more testing has
been done, it will be released in 1.0.

Roadmap
-------

See [the roadmap](ROADMAP.md) for the current plan. Help on any of
these points is welcome!

FAQ
---

### Why do many Jbuilder projects contain a Makefile?

Many Jbuilder project contain a toplevel `Makefile`. It is often only
there for convenience, for the following reasons:

1. there are many different build systems out there, all with a
   different CLI. If you have been hacking for a long time, the one
   true invocation you know is `make && make install`, possibly
   preceded by `./configure`

2. you often have a few common operations that are not part of the
   build and `make <blah>` is a good way to provide them

3. `make` is shorter to type than `jbuilder build @install`

### How to add a configure step to a jbuilder project?

[example/sample-projects/with-configure-step](example/sample-projects/with-configure-step) shows
one way to do it which preserves composability; i.e. it doesn't require manually
running `./configure` script when working on multiple projects at the same time.

### Can I use topkg with jbuilder?

Yes, have a look at the [topkg-jbuilder][topkg-jbuilder] project for
more details.

Known issues
------------

### Optional libraries inside a multilib directory

https://github.com/janestreet/jbuilder/issues/51

If a directory contains several libraries and some are marked as
optional (by adding `(optional)` in the `(library ...)` stanza), then
the dependencies will still be required to perform the build.

This could be sorted out with some refactoring, but there is a simple
workaround, so it is low-priority.

#### Workaround

Put each optional library in a separate directory.

### mli only modules

https://github.com/janestreet/jbuilder/issues/9

Due to the low-level details of OCaml compilation, it is currently
possible to write a module that has only a `.mli` and no `.ml`
file. This works as long as the mli contains only type declarations.

This is not a properly supported feature of the compiler, and in
particular it is not possible to alias such modules or use them as the
argument of a functor. Moreover, if you do write a value declaration,
or even just define an exception in the `.mli`, then you won't get an
error until the point where you link an executable using this module.

For these reason, mli only modules are not recommended by Jbuilder
until the compiler support them properly.

#### Workaround

As long as a module type contains no value declaration, it is possible
to turn in to an implementation by using a recursive module:

```ocaml
module rec M : sig
  type t = A | B
end = M
include M
```

So if you have a module without a `.ml` file, simply generate a `.ml`
from the `.mli` using this trick. For instance you can add the
following rule into your jbuild file:

```scheme
(rule (with-output-to foo.ml
       (progn
        (echo "module rec HACK : sig\n")
        (cat foo.mli)
        (echo "\nend = HACK\ninclue HACK\n"))))
```

In fact, jbuilder will automatically add this rule if you have a
module without imlpementation. However it will print a warning.

Implementation details
----------------------

This section is for people who want to work on Jbuilder itself.

### Bootstrap

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

### OCaml compatibility test

Install opam switches for all the entries in the
[jbuild-workspace.dev](jbuild-workspace.dev) file and run:

```sh
$ make all-supported-ocaml-versions
```

### Repository organization

- `vendor/` contains dependencies of Jbuilder, that have been vendored
- `plugin/` contains the API given to `jbuild` files that are OCaml
  scripts
- `src/` contains the core of `Jbuilder`, as a library so that it can
  be used to implement the Jenga bridge later
- `bin/` contains the command line interface
- `doc/` contains the manual and rules to generate the manual pages

### Design

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

#### Code flow

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

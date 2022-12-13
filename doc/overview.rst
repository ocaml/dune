********
Overview
********

Introduction
============

Dune is a build system for OCaml (with support for Reason and Coq). It is not
intended as a completely generic build system that's able to build any project
in any language. On the contrary, it makes lots of choices in order to encourage
a consistent development style.

This scheme is inspired from the one used inside Jane Street and adapted to the
opam world. It has matured over a long time and is used daily by hundreds of
developers, which means that it is highly tested and productive.

When using Dune, you give very little, high-level information to the build
system, which in turn takes care of all the low-level details from the
compilation of your libraries, executables, and documentation to the
installation, setting up of tests, and setting up development tools such as
Merlin, etc.

In addition to the normal features expected from an OCaml build system, Dune
provides a few additional ones that separate it from the crowd:

-  You never need to tell Dune the location of things such as libraries. Dune
   will discover them automatically. In particular, this means that when you
   want to reorganise your project, you need nothing other than to rename your
   directories, Dune will do the rest.

-  Things always work the same whether your dependencies are local or installed
   on the system. In particular, this means that you can insert the source for a
   project dependency in your working copy, and Dune will start using it
   immediately. This makes Dune a great choice for multi-project development.

-  Cross-platform: as long as your code is portable, Dune will be able to
   cross-compile it. Read more in the :ref:`cross-compilation` section.

-  Release directly from any revision: Dune needs no setup stage. To release
   your project, simply point to a specific Git tag (named revision). Of course,
   you can add some release steps if you'd like, but it isn't necessary. For
   more information, please refer to `dune-release
   <https://github.com/samoht/dune-release>`_.

The first section below defines some terms used in this manual. The second
section specifies the Dune metadata format, and the third one describes how to
use the ``dune`` command.

Terminology
===========

-  **root**: the top-most directory in a GitHub repo, workspace, and project,
   differentiated by variables such as `%{workspace_root}` and
   `%{project_root}`. Dune builds things from this directory. It knows how to
   build targets that are descendants of the root. Anything outside of the tree
   starting from the root is considered part of the **installed world**. Refer
   to :ref:`finding-root` to learn how the workspace root is determined.

-  **workspace**: the subtree starting from each root. It can contain any number
   of projects that will be built simultaneously by Dune, and it must contain a
   `dune-workspace` file.

-  **project**: a collection of source files that must include a `dune-project`
   file. It may also contain one or more packages. Each directory in the tree,
   including the root, must have a `dune` file specifying how to build the files
   in its directory. Projects can be shared between different applications.

-  **package**: a set of libraries and executables that opam builds and installs
   as one.

-  **installed world**: anything outside of the workspace. Dune doesn't know how
   to build things in the installed world.

-  **installation**: the action of copying build artifacts or other files from
   the ``<root>/_build`` directory to the installed world.

-  **scope**: defined by any directory that contains at least one
   `<package>.opam` file. Typically, every project defines a single scope that
   is a subtree starting from this directory. Moreover, scopes are separate from
   your project's dependencies. The scope also determines where private items
   are visible. Private items include libraries or binaries that will not be
   installed.  See :ref:`scopes` for more details.

-  **build context**: a specific configuration written in a
   :ref:`dune-workspace` file, which has a corresponding subdirectory in the
   ``<root>/_build`` directory. It contains all the workspace's build artifacts.
   Without this specific configuration from the user, there is always a
   ``default`` build context that corresponds to the executed Dune environment. 

-  **build context root**: the root of a build context named ``foo`` is
   ``<root>/_build/<foo>``.

-  **build target**: specified on the command line, e.g., `dune build
   <target_path.exe>`. All targets that Dune knows how to build live in the
   `_build` directory.

- **alias**: a build target that doesn't produce any file and has configurable
  dependencies. Targets starting with `@` on the command line are interpreted as
  aliases (e.g., `dune build @src/runtest`). Aliases are per-directory. However,
  asking to build an alias in a given directory will also trigger alias
  construction in all children directories recursively. If no target is
  specified, Dune builds the `default` alias.  Dune defines several
  :ref:`builtin-aliases`.

- **environment**: determines the default values of various parameters, such as
  the compilation flags. In Dune, each directory has an environment attached to
  it. Inside a scope, each directory inherits the environment from its parent.
  At the root of every scope, a default environment is used. At any point, the
  environment can be altered using an :ref:`dune-env` stanza.

- **build profile**: a global setting that influences various defaults. It can
  be set from the command line using ``--profile <profile>`` or from
  ``dune-workspace`` files. The following profiles are standard:

  -  ``release`` which is the profile used for opam releases
  -  ``dev`` which is the default profile when none is set explicitly, it has
     stricter warnings than the ``release`` one

Project Layout
==============

A typical Dune project will have a ``dune-project`` and one or more
``<package>.opam`` files at the root as well as ``dune`` files wherever
interesting things are: libraries, executables, tests, documents to install,
etc.

We recommended organising your project to have exactly one library per
directory. You can have several executables in the same directory, as long as
they share the same build configuration. If you'd like to have multiple
executables with different configurations in the same directory, you will have
to make an explicit module list for every executable using ``modules``.

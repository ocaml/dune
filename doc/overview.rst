********
Overview
********

Introduction
============

Dune is a build system for OCaml (with support for Reason and Coq).
It is not intended as a completely generic build system that's able
to build any project in any language. On the contrary, it makes
lots of choices in order to encourage a consistent development style.

This scheme is inspired from the one used inside Jane Street and adapted
to the opam world. It has matured over a long time and is used daily by
hundreds of developers, which means that it is highly tested and
productive.

When using Dune, you give very little, high-level information to
the build system, which in turn takes care of all the low-level
details from the compilation of your libraries, executables, and
documentation to the installation, setting up of tests, and setting up 
development tools such as Merlin, etc.

In addition to the normal features expected from an OCaml build system, 
Dune provides a few additional ones that separate it from
the crowd:

-  You never need to tell Dune the location of things such as libraries.
   Dune will discover them automatically. In particular, this
   means that when you want to re-organize your project, you need nothing other 
   than to rename your directories, Dune will do the rest.

-  Things always work the same whether your dependencies are local or
   installed on the system. In particular, this means that you can 
   insert the source for a project dependency in your working
   copy, and Dune will start using it immediately. This makes Dune a
   great choice for multi-project development.

-  Cross-platform: as long as your code is portable, Dune will be
   able to cross-compile it (note that Dune is designed internally
   to make this easy, but the actual support is not implemented yet)

-  Release directly from any revision: Dune needs no setup stage. To
   release your project, simply point to a specific tag. Of course, you can 
   add some release steps if you'd like, but it isn't
   necessary. 

The first section below defines some terms used in 
this manual. The second section specifies the Dune metadata
format, and the third one describes how to use the ``dune`` command.

Terminology
===========

-  **package**: a set of libraries and executables that
   opam builds and installs as one

-  **project**: a source tree, maybe containing one or more
   packages

-  **root**: the directory from where Dune can build
   things. Dune knows how to build targets that are descendants of
   the root. Anything outside of the tree starting from the root is
   considered part of the **installed world**. How the root is
   determined is explained in :ref:`finding-root`.

-  **workspace**: the subtree starting from the root.
   It can contain any number of projects that will be built
   simultaneously by Dune.

-  **installed world**: anything outside of the workspace, that Dune
   takes for granted and doesn't know how to build

-  **installation**: the action of copying build artifacts or
   other files from the ``<root>/_build`` directory to the installed
   world

-  **scope**: determines where private items are
   visible. Private items include libraries or binaries that will not
   be installed. In Dune, scopes are subtrees rooted where at
   least one ``<package>.opam`` file is present. Moreover, scopes are
   exclusive. Typically, every project defines a single scope. See
   :ref:`scopes` for more details.

-  **build context**: a subdirectory of the
   ``<root>/_build`` directory. It contains all the build artifacts of
   the workspace built against a specific configuration. Without
   specific configuration from the user, there is always a ``default``
   build context, which corresponds to the environment in which Dune
   executes. Build contexts can be specified by writing a
   :ref:`dune-workspace` file.

-  **build context root**: the root of a build context named ``foo`` is
   ``<root>/_build/<foo>``

- **alias**: a build target that doesn't produce any file and has
  configurable dependencies. Aliases are per-directory. However, on the command
  line, asking to build an alias in a given directory will trigger the
  construction of the alias in all children directories recursively. Dune
  defines several :ref:`builtin-aliases`.

- **environment**: in Dune, each directory has an environment
  attached to it. The environment determines the default values of
  various parameters, such as the compilation flags. Inside a scope,
  each directory inherits the environment from its parent. At the root
  of every scope, a default environment is used. At any point, the
  environment can be altered using an :ref:`dune-env` stanza.

- **build profile**: a global setting that influences various
  defaults. It can be set from the command line using ``--profile
  <profile>`` or from ``dune-workspace`` files. The following
  profiles are standard:

  -  ``release`` which is the profile used for opam releases
  -  ``dev`` which is the default profile when none is set explicitly, it
     has stricter warnings than the ``release`` one

Project Layout
==============

A typical Dune project will have a ``dune-project`` and one or more
``<package>.opam`` files at the root as well as ``dune`` files wherever
interesting things are: libraries, executables, tests, documents to install,
etc.

We recommended organizing your project to have exactly one library
per directory. You can have several executables in the same directory, as long
as they share the same build configuration. If you'd like to have multiple
executables with different configurations in the same directory, you will have
to make an explicit module list for every executable using ``modules``.

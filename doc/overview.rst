********
Overview
********

Introduction
============

Dune is a build system for OCaml (with support for Reason and Coq).
It is not intended as a completely generic build system that is able
to build any given project in any language.  On the contrary, it makes
lots of choices in order to encourage a consistent development style.

This scheme is inspired from the one used inside Jane Street and adapted
to the opam world. It has matured over a long time and is used daily by
hundreds of developers, which means that it is highly tested and
productive.

When using dune, you give very little and high-level information to
the build system, which in turn takes care of all the low-level
details, from the compilation of your libraries, executables and
documentation, to the installation, setting up of tests, setting up of
the development tools such as merlin, etc.

In addition to the normal features one would expect from a build system
for OCaml, dune provides a few additional ones that detach it from
the crowd:

-  you never need to tell dune where things such as libraries are.
   Dune will always discover them automatically. In particular, this
   means that when you want to re-organize your project you need to do no
   more than rename your directories, dune will do the rest

-  things always work the same whether your dependencies are local or
   installed on the system. In particular, this means that you can always
   drop in the source for a dependency of your project in your working
   copy and dune will start using it immediately. This makes dune a
   great choice for multi-project development

-  cross-platform: as long as your code is portable, dune will be
   able to cross-compile it (note that dune is designed internally
   to make this easy but the actual support is not implemented yet)

-  release directly from any revision: dune needs no setup stage. To
   release your project, you can simply point to a specific tag. You can
   of course add some release steps if you want to, but it is not
   necessary

The first section of this document defines some terms used in the rest
of this manual. The second section specifies the dune metadata
format and the third one describes how to use the ``dune`` command.

Terminology
===========

-  **package**: a package is a set of libraries, executables, ... that
   are built and installed as one by opam

-  **project**: a project is a source tree, maybe containing one or more
   packages

-  **root**: the root is the directory from where dune can build
   things. Dune knows how to build targets that are descendants of
   the root. Anything outside of the tree starting from the root is
   considered part of the **installed world**. How the root is
   determined is explained in :ref:`finding-root`.

-  **workspace**: the workspace is the subtree starting from the root.
   It can contain any number of projects that will be built
   simultaneously by dune

-  **installed world**: anything outside of the workspace, that dune
   takes for granted and doesn't know how to build

-  **installation**: this is the action of copying build artifacts or
   other files from the ``<root>/_build`` directory to the installed
   world

-  **scope**: a scope determines where private items are
   visible. Private items include libraries or binaries that will not
   be installed. In dune, scopes are sub-trees rooted where at
   least one ``<package>.opam`` file is present. Moreover, scopes are
   exclusive. Typically, every project defines a single scope. See
   :ref:`scopes` for more details

-  **build context**: a build context is a subdirectory of the
   ``<root>/_build`` directory. It contains all the build artifacts of
   the workspace built against a specific configuration. Without
   specific configuration from the user, there is always a ``default``
   build context, which corresponds to the environment in which dune
   is executed. Build contexts can be specified by writing a
   :ref:`dune-workspace` file

-  **build context root**: the root of a build context named ``foo`` is
   ``<root>/_build/<foo>``

- **alias**: an alias is a build target that doesn't produce any file and has
  configurable dependencies. Aliases are per-directory. However, on the command
  line, asking for an alias to be built in a given directory will trigger the
  construction of the alias in all children directories recursively. Dune
  defines several :ref:`builtin-aliases`.

- **environment**: in dune, each directory has an environment
  attached to it. The environment determines the default values of
  various parameters, such as the compilation flags. Inside a scope,
  each directory inherit the environment from its parent. At the root
  of every scope, a default environment is used. At any point, the
  environment can be altered using an :ref:`dune-env` stanza.

- **build profile**: a global setting that influence various
  defaults. It can be set from the command line using ``--profile
  <profile>`` or from ``dune-workspace`` files. The following
  profiles are standard:

  -  ``release`` which is the profile used for opam releases
  -  ``dev`` which is the default profile when none is set explicitly, it
     has stricter warnings that the ``release`` one

Project layout
==============

A typical dune project will have a ``dune-project`` and one or more
``<package>.opam`` file at the root as well as ``dune`` files wherever
interesting things are: libraries, executables, tests, documents to install,
etc...

It is recommended to organize your project so that you have exactly one library
per directory. You can have several executables in the same directory, as long
as they share the same build configuration. If you'd like to have multiple
executables with different configurations in the same directory, you will have
to make an explicit module list for every executable using ``modules``.

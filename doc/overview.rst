********
Overview
********

.. TODO(diataxis)

   Split into:

   - info on the index page
   - :doc:`glossary`
   - a history page that could also explain the various actors

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
   more information, please refer to dune-release_.

.. _dune-release: https://github.com/tarides/dune-release

The first section below defines some terms used in this manual. The second
section specifies the Dune metadata format, and the third one describes how to
use the ``dune`` command.

Project Layout
==============

A typical Dune project will have a ``dune-project``, as well as ``dune`` files wherever
interesting things are: libraries, executables, tests, documents to install,
etc.

We recommend organising your project to have exactly one library per
directory. You can have several executables in the same directory, as long as
they share the same build configuration. If you'd like to have multiple
executables with different configurations in the same directory, you will have
to make an explicit module list for every executable using ``modules``.

History
=======

Dune started as ``jbuilder`` in late 2016. When its 1.0.0 version was released
in 2018, the name has been changed to ``dune``. It used to be configured with
``jbuild`` and ``jbuild-workspace`` files with a slightly different syntax.
After a transition period, this syntax is not supported anymore.

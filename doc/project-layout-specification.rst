*****************************************
Project Layout and Metadata Specification
*****************************************

A typical dune project will have a ``dune-project`` and one or more
``<package>.opam`` file at the root as well as ``dune`` files wherever
interesting things are: libraries, executables, tests, documents to install,
etc...

It is recommended to organize your project so that you have exactly one library
per directory. You can have several executables in the same directory, as long
as they share the same build configuration. If you'd like to have multiple
executables with different configurations in the same directory, you will have
to make an explicit module list for every executable using ``modules``.

.. _opam-files:

<package>.opam files
====================

When a ``<package>.opam`` file is present, dune will know that the
package named ``<package>`` exists. It will know how to construct a
``<package>.install`` file in the same directory to handle installation
via `opam <https://opam.ocaml.org/>`__. Dune also defines the
recursive ``install`` alias, which depends on all the buildable
``<package>.install`` files in the workspace. So for instance to build
everything that is installable in a workspace, run at the root:

::

    $ dune build @install

Declaring a package this way will allow you to add elements such as libraries,
executables, documentation, ... to your package by declaring them in ``dune``
files.

Such elements can only be declared in the scope defined by the
corresponding ``<package>.opam`` file. Typically, your
``<package>.opam`` files should be at the root of your project, since
this is where ``opam pin ...`` will look for them.

Note that ``<package>`` must be non-empty, so in particular ``.opam``
files are ignored.

.. _scopes:

Scopes
------

Any directory containing at least one ``<package>.opam`` file defines
a scope. This scope is the sub-tree starting from this directory,
excluding any other scopes rooted in sub-direcotries.

Typically, any given project will define a single scope. Libraries and
executables that are not meant to be installed will be visible inside
this scope only.

Because scopes are exclusive, if you wish to include the dependencies
of the project you are currently working on into your workspace, you
may copy them in a ``vendor`` directory, or any other name of your
choice. Dune will look for them there rather than in the installed
world and there will be no overlap between the various scopes.

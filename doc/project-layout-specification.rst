**************
Project layout
**************

A typical dune project will have a ``dune-project`` and one or more
``<package>.opam`` file at the root as well as ``dune`` files wherever
interesting things are: libraries, executables, tests, documents to install,
etc...

It is recommended to organize your project so that you have exactly one library
per directory. You can have several executables in the same directory, as long
as they share the same build configuration. If you'd like to have multiple
executables with different configurations in the same directory, you will have
to make an explicit module list for every executable using ``modules``.

The next sections describe the format of dune metadata files.

Note that the dune metadata format is versioned in order to ensure forward
compatibility. There is currently only one version available, but to be future
proof, you should still specify it in your ``dune`` files. If no version is
specified, the latest one will be used.

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

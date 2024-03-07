Scopes
======

.. TODO(diataxis)
   - reference: library lookup
   - howto: vendoring

Any directory containing at least one ``<package>.opam`` file defines
a scope. This scope is the subtree starting from this directory,
excluding any other scopes rooted in subdirectories.

Typically, any given project will define a single scope. Libraries and
executables that aren't meant to be installed will be visible inside
this scope only.

Because scopes are exclusive, if you wish to include your current project's
dependencies in your workspace, you can copy them in a ``vendor`` directory,
or any name of your choice. Dune will look for them there rather than in the
:term:`installed world`, and there will be no overlap between the various
scopes.

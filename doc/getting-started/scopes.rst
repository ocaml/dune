Scopes
======

.. TODO(diataxis)
   - reference: library lookup
   - howto: vendoring

Whenever Dune builds anything, it does so at the level of a *Dune workspace*. A
Dune workspace is a set of Dune projects. A typical workspace consists of a
single Dune project.

A Dune project is defined by the presence of a
:doc:`/reference/dune-project/index` file. Each Dune project extends over the
subtree rooted at the directory containing the
:doc:`/reference/dune-project/index` file, excluding any nested Dune projects.

The root of the workspace is defined by the outermost
:doc:`/reference/dune-project/index` file starting from the current directory,
or by the first :doc:`/reference/dune-workspace/index` file found when climbing
up the directory hierachy starting from the current directory (see
:ref:`finding-root`). It can also be specified explicitly by passing the
``--root`` flag (see :ref:`forcing-root`).

Different Dune projects within the same Dune workspace are completely
independent and no settings are shared between them, even if they are nested
within each other.

Settings in :doc:`/reference/dune-workspace/index`, on the other hand, are
inherited by all Dune projects in the workspace.

Only public items (public libraries, public executables) from a Dune project are
visible to other Dune projects within the same Dune workspace.

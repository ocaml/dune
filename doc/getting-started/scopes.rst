Dune projects and workspaces
============================

Whenever Dune builds anything, it does so at the level of a *Dune workspace*. A
Dune workspace is a set of Dune projects. A typical workspace consists of a
single Dune project.

A Dune project is defined by the presence of a
:doc:`/reference/dune-project/index` file. Each Dune project extends over the
subtree rooted at the directory containing the
:doc:`/reference/dune-project/index` file, excluding any nested Dune projects.

The root of the current workspace is determined by the outermost
:doc:`/reference/dune-project/index` file in an ancestor of the current
directory or by the presence of a :doc:`/reference/dune-workspace/index` file
(see :ref:`finding-root` and :ref:`forcing-root` for details). All
:doc:`/reference/dune-workspace/index` files other than the one specifying the
root of the workspace are ignored.

Different Dune projects within the same Dune workspace are independent of each
other and no settings are shared between them, even if they are nested within
each other.

Settings in :doc:`/reference/dune-workspace/index`, on the other hand, are
inherited by all Dune projects in the workspace.

Within a Dune project, :doc:`/reference/dune/index` files are used to define
all objects of interest for Dune: libraries, executables, tests, etc. Only
public items (public libraries, public executables) from a Dune project are
visible to other Dune projects within the same Dune workspace.

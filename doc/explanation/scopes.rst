Dune Projects and Workspaces
============================

Whenever Dune builds anything, it does so at the level of a *Dune workspace*. A
Dune workspace is a set of Dune projects. A typical workspace consists of a
single Dune project.

A *Dune project* is defined by the presence of a
:doc:`/reference/dune-project/index` file. Each Dune project extends over the
file tree rooted at the directory containing the
:doc:`/reference/dune-project/index` file, excluding any nested Dune projects.

Dune determines the root of the current workspace by finding the topmost
ancestor containing a :doc:`/reference/dune-project/index` file or by the
presence of a :doc:`/reference/dune-workspace/index` file (see
:ref:`finding-root` and :ref:`forcing-root` for details).

Different Dune projects within the same Dune workspace are independent of each
other and no settings are shared between them, even if they are nested within
each other.

Settings in :doc:`/reference/dune-workspace/index`, on the other hand, are
inherited by all Dune projects in the workspace. Some settings (those that make
sense for all projects) can be specified both in
:doc:`/reference/dune-project/index` and :doc:`/reference/dune-workspace/index`
files, with the former taking precedence. Note that all
:doc:`/reference/dune-workspace/index` files other than the one specifying the
root of the workspace are ignored.

Within a Dune project, :doc:`/reference/dune/index` files are used to define all
objects of interest for Dune: libraries, executables, tests, etc. There are
typically many :doc:`/reference/dune/index` files in a Dune project: one per
directory, unless the directory does not contain anything relevant to Dune. In
each :doc:`/reference/dune/index` file, references are resolved relative to the
directory containing the file.

Note that there are specific stanzas and actions that may result in exceptions
to some of the rules stated in the previous paragraph. See, for example, the
:doc:`/reference/dune/subdir` and :doc:`/reference/dune/include_subdirs`
stanzas, as well as the :doc:`/reference/actions/chdir` action.

Finally, note that only public items (public libraries, public executables) of a
Dune project are visible to other Dune projects within the same Dune workspace.

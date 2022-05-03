*********
Migration
*********

Dune was initially called Jbuilder. Up to mid-2018, the package was still
called Jbuilder, which only installed a ``jbuilder`` binary. This document
explains how the migration to Dune will happen.

Timeline
========

The general idea is that the migration is gradual, and existing Jbuilder
projects don't need to be updated all at once. We encourage users to switch
their development repositories and continue their usual release cycle. There is
no need to rerelease existing packages just to switch to Dune immediately.

The plan is as follows:

July 2018: Release of Dune 1.0.0
--------------------------------

First, the release of the opam package `dune`: the `jbuilder` package becomes a
transitional package that depends on `dune`.

The `dune` package installs two binaries: ``dune`` and ``jbuilder``. These two
identical binaries work on both Jbuilder and Dune projects. Additionally, they
recognize both Jbuilder and Dune configuration files. The new Dune
configuration files are described later in this document.

January 2019: Deprecation of Jbuilder
-------------------------------------

At this point, the ``jbuilder`` binary emits a warning on every startup,
inviting users to switch to ``dune``. When encountering ``jbuild`` or other
Jbuilder configuration files, both binaries emit a warning. The rest remains
unchanged.

During this period, it makes sense for projects to do new releases just to
switch to Dune if none of their existing releases use Dune.

July 2019: Support for Jbuilder is Dropped
------------------------------------------

`jbuilder`, now a dummy executable, always throws an error message on startup.
Dune no longer reads `jbuild` or other Jbuidler configuration files, but it
still prints a warning when encountering them.

At this point, a conflict with newer versions of Dune will be added
to all opam packages that rely on the ``jbuilder`` binary or Jbuilder
configuration files.

January 2020: The ``jbuilder`` Binary Goes Away
-----------------------------------------------

The ``dune`` package no longer installs a ``jbuilder`` binary. The rest is
unchanged.

Distant Future
--------------

Once we're sure there are no more ``jbuild`` files out there, Dune will
completely ignore ``jbuild`` and other Jbuilder configuration files.

Checklist
=========

You can find a concise list of migration tasks that will be required to
transition from Jbuilder to Dune below:

New Configuration Files
-----------------------

Until July 2019, Dune will still read ``jbuild`` and other Jbuilder
configuration files. There is no change in these files.

However, based on the experience acquired since the first release of
Jbuilder, we made a few changes in the configuration files read by
Dune. The most notable ones are the following:

- ``jbuild`` files are renamed simply ``dune``.
- projects now have a ``dune-project`` file at their root
- ``jbuild-ignore`` files are replaced by ``ignored_subdirs`` stanzas in
  ``dune`` files.
- ``jbuild-workspace`` are replaced by ``dune-workspace`` files.
- ``jbuild-workspace<suffix>`` files no longer mean anything.

Detailed explanations of the differences between the
Jbuilder and Dune configuration files follow:

``dune-project`` Files
----------------------

These are a new kind of file. With Jbuilder, projects used to be
identified by the presence of at least one ``<package>.opam`` file in a
directory. This will still be supported until July 2019; however, as
Jbuilder evolved, it became clear that we needed project files, so Dune
introduced ``dune-project`` files to mark the root of projects.

Eventually, we hope that Dune will generate ``opam`` files, so users
will only have to write a ``dune-project`` file.

The purpose of this file is to:

- delimit projects in larger workspaces
- set a few project-wide parameters, such as the name, the version of the Dune
  language in use, or specification of extra features (plugins) used in the
  project

Eventually, for users who wish to do so, it should be possible to
centralize all the project's configurations in this file.

``dune`` Files
--------------

These are the same as ``jbuild`` files.

``dune-workspace`` Files
------------------------

These are the same as ``jbuild-workspace`` files.

When looking for the root of the workspace, Jbuilder also looks for
files whose name start with ``jbuild-workspace``, such as
``jbuild-workspace.in``. This rule will be kept until July 2019; however,
it's not preserved for ``dune-workspace`` files (i.e., a
``dune-workspace.in`` file means nothing).

This rule was only useful when we didn't have project files.

Variable Syntax
---------------

``${foo} and $(foo)`` are no longer valid variable syntax in ``dune`` files.
Variables are defined as ``%{foo}``. This change simplifies 
interoperability with bash commands that also use the ``${foo}`` syntax.

``(files_recursively_in ..)`` is Removed
----------------------------------------

The ``files_recursively_in`` dependency specification is invalid in ``dune`` files.
A :ref:`source_tree <source_tree>` stanza has been introduced to reflect the
actual function of this stanza.

Escape Sequences
----------------

Invalid escape sequences of the form ``\x`` where ``x`` is a character other
than ``[0-9]``, ``x``, ``n``, ``r``, ``t``, ``b`` are not allowed in ``dune`` files.

Comments Syntax
---------------

Block comments of the form ``#| ... |#`` and comments of the form ``#;`` are not
supported in ``dune`` files.

Renamed Variables
-----------------

All existing variables have been lowercased for consistency. Other variables
have always been renamed. Refer to this table for details:

======================== ============
Jbuild                    Dune
======================== ============
``${@}``                  ``%{targets}``
``${^}``                  ``%{deps}``
``${path:file}``          ``%{dep:file}``
``${SCOPE_ROOT}``         ``%{project_root}``
``${ROOT}``               ``%{workspace_root}``
``${findlib:..}``         ``%{lib:..}``
``${CPP}``                ``%{cpp}``
``${CC}``                 ``%{cc}``
``${CXX}``                ``%{cxx}``
``${OCAML}``              ``%{ocaml}``
``${OCAMLC}``             ``%{ocamlc}``
``${OCAMLOPT}``           ``%{ocamlopt}``
``${ARCH_SIXTYFOUR}``     ``%{arch_sixtyfour}``
``${MAKE}``               ``%{make}``
======================== ============

Removed Variables
-----------------

``${path-no-dep:file}`` and ``${<}`` have been removed.

A named dependency should be used instead of ``${<}``. For instance
the following ``jbuild`` file:

.. code:: lisp

          (alias
           ((name   runtest)
            (deps   (input))
            (action (run ./test.exe %{<}))))

should be rewritten to the following ``dune`` file:

.. code:: lisp

          (rule
           (alias  runtest)
           (deps   (:x input))
           (action (run ./test.exe %{x})))

``# JBUILDER_GEN`` Renamed
--------------------------

``# DUNE_GEN`` should be used instead of ``# JBUILDER_GEN`` in META templates.


``jbuild-ignore`` (Deprecated)
------------------------------

``jbuild-ignore`` files are deprecated and replaced by :ref:`dune-subdirs`
stanzas in ``dune`` files.

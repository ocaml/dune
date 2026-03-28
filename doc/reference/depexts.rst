External System Dependencies (depexts)
======================================

Depexts (external dependencies) are system packages that OCaml packages may
require at build or run time. Common examples include C libraries, system
tools, and ``pkg-config`` packages. They are declared in the `depexts field
of opam files <https://opam.ocaml.org/doc/Manual.html#opamfield-depexts>`_
and Dune reads and surfaces this information during dependency solving.

Declaration in Opam Files
-------------------------

Depexts are declared using the ``depexts:`` field in opam files. Each entry is
a list of system package names with an optional filter condition:

.. code:: opam

   depexts: [
     ["pkg-config"]
     ["libsqlite3-dev"] {os-family = "debian"}
     ["sqlite"] {os = "macos"}
   ]

Entries without a filter apply unconditionally. Entries with a filter apply
only when the filter condition matches the current platform. Filter conditions
use `opam's variable names
<https://opam.ocaml.org/doc/Manual.html#opamfield-depexts>`_ (e.g.,
``os-family``, ``os-distribution``, ``arch``).

Dune reads this field from opam package metadata when solving dependencies
(either via ``dune pkg lock`` or automatically) and records the result in the
lock directory.

.. note::

   The ``depexts`` field cannot be set in the ``(package)`` stanza of a
   ``dune-project`` file. To declare depexts for your own packages, add them
   to a ``.opam.template`` file, whose fields are merged into the generated
   opam file. See :doc:`/reference/packages` for details on opam file
   generation.

Querying Depexts
----------------

.. describe:: dune show depexts [--context CONTEXT]

   Print the system packages required by the current project's dependencies,
   including transitive dependencies. Each system package name is printed on
   its own line, sorted alphabetically. Duplicate names across packages are
   removed.

   ``--context CONTEXT``
      Select which build context's dependencies to query. Defaults to
      ``default``.

   .. code:: console

      $ dune show depexts
      gnupg
      unzip

Build Failure Hints
-------------------

When a locked package fails to build and that package has associated depexts,
Dune prints a hint listing that package's depexts after the error message
(unlike ``dune show depexts``, which aggregates depexts across all packages):

.. Verified against test/blackbox-tests/test-cases/pkg/depexts/error-message.t

.. code:: console

   Error: Program pkg-config not found in the tree or in PATH
    (context: default)
   Hint: You may want to verify the following depexts are installed:
   - gnupg
   - unzip

This hint is informational. Dune does not install or manage system packages.

.. seealso::

   `opam Manual: depexts field <https://opam.ocaml.org/doc/Manual.html#opamfield-depexts>`_
      Specification of the ``depexts`` field in opam files.

   :doc:`/explanation/package-management`
      Conceptual overview of Dune's package management.

   :doc:`/reference/packages`
      Package definitions and opam file generation.

.. TODO: link to explanation section on depexts once added to
   doc/explanation/package-management.md (see #13657)

.. TODO: link to tutorial on depexts once added to
   doc/tutorials/dune-package-management/ (see #13657)

.. TODO: link to how-to guide on handling external dependencies once
   doc/howto/handle-external-dependencies.md is created (see #13657)

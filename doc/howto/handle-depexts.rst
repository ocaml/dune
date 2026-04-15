How to Handle External System Dependencies (depexts)
====================================================

This guide shows how to declare and query external system dependencies
(depexts) — system packages like C libraries or tools that OCaml packages
require at build or run time.

Checking What System Packages Are Needed
----------------------------------------

.. note::

   This section applies to projects using Dune's package management. If you
   manage dependencies with opam, use ``opam list --required-by <pkg>
   --external`` to query depexts instead.

Once you have a lock directory (created by running ``dune pkg lock`` or
automatically when building with package management enabled), you can list all
required system packages:

.. code:: console

   $ dune show depexts
   gnupg
   unzip

This prints the system package names from all locked packages, deduplicated and
sorted. Install these packages using your system's package manager (e.g.,
``apt``, ``brew``, ``dnf``) before building.

Declaring Depexts for Your Own Packages
---------------------------------------

Declaring depexts makes your package's system dependencies visible to
consumers, whether they use Dune package management or opam.

The ``depexts`` field is not yet supported in the ``(package)`` stanza of
``dune-project``. To declare depexts for your own packages, add them to a
``.opam.template`` file:

1. Create a file named ``<package-name>.opam.template`` in your project root.

2. Add the ``depexts`` field using opam syntax:

   .. code:: opam

      depexts: [
        ["pkg-config"]
        ["libsqlite3-dev"] {os-family = "debian"}
        ["sqlite"] {os = "macos"}
      ]

3. When opam files are generated (via ``dune build`` with
   ``(generate_opam_files)`` in ``dune-project``), the template fields are
   merged into the generated opam file.

If you maintain ``.opam`` files manually (without ``(generate_opam_files)``),
add the ``depexts`` field directly to the ``.opam`` file instead.

See :doc:`/reference/packages` for more on opam file generation and templates.

Diagnosing Build Failures
-------------------------

.. note::

   Build failure hints for depexts only appear when using Dune's package
   management. When using opam, depexts are typically installed automatically
   by ``opam depext`` or ``opam install``.

When a locked package fails to build and has associated depexts, Dune prints a
hint after the error:

.. code:: console

   Error: Program pkg-config not found in the tree or in PATH
    (context: default)
   Hint: You may want to verify the following depexts are installed:
   - gnupg
   - unzip

If you see this hint, install the listed system packages and retry the build.

.. seealso::

   :doc:`/explanation/package-management`
      How Dune's package management works, including how depexts are resolved.

   `opam Manual: depexts field <https://opam.ocaml.org/doc/Manual.html#opamfield-depexts>`_
      Specification of the ``depexts`` field in opam files.

depexts
=======

.. warning::

   :doc:`Dune Package Management </explanation/package-management>` is not
   final yet and the features described here are subject to change.

Depexts (external dependencies) are system packages that OCaml packages may
require at build or run time. Common examples include C libraries, system
tools, and ``pkg-config`` packages. They are declared in opam package metadata
and recorded in the lock directory during dependency resolution.

.. versionadded:: 3.13

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
use opam's own variable names (with hyphens, e.g., ``os-distribution``).

Dune reads this field from opam package metadata during ``dune pkg lock`` and
records the result in the lock directory.

.. note::

   The ``depexts`` field cannot be set in the ``(package)`` stanza of a
   ``dune-project`` file. To declare depexts for your own packages, add them
   to a ``.opam.template`` file, whose fields are merged into the generated
   opam file. See :doc:`/reference/packages` for details on opam file
   generation.

Lock Directory Format
---------------------

When locking dependencies with ``dune pkg lock``, depexts are stored in
``.pkg`` files within the lock directory.

.. describe:: (depexts <entry> ...)

   .. versionadded:: 3.13

   Records the system packages that a locked package depends on.

   In a standard lock directory, each entry is a plain system package name.
   Depexts are resolved for the current platform at solve time, so only the
   names relevant to the solving platform are stored:

   .. code:: dune

      (depexts unzip gnupg)

   In a portable lock directory (enabled by setting
   ``DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled``), filter conditions from the
   opam file are converted to
   :doc:`boolean language </reference/boolean-language>` form so that the
   correct system packages can be determined at build time on any supported
   platform. Each entry is one of:

   - A list of package names, which applies unconditionally.
   - A list ``(<names> <condition>)`` where ``<names>`` is a list of package
     names and ``<condition>`` is a boolean language expression.

   .. code:: dune

      (depexts
       (foo bar baz)
       ((foo-ubuntu bar-ubuntu)
        (= %{os_distribution} ubuntu))
       ((foo-arch bar-arch)
        (= %{os_distribution} archlinux)))

   Note that variable names in lock file conditions use underscores (e.g.,
   ``%{os_distribution}``), while the corresponding opam filter variables use
   hyphens (e.g., ``os-distribution``).

Querying Depexts
----------------

.. describe:: dune show depexts [--context CONTEXT]

   .. versionadded:: 3.13

   Print the system packages required by the current project's locked
   dependencies, including transitive dependencies. Each system package name
   is printed on its own line, sorted alphabetically. Duplicate names across
   packages are removed.

   A lock directory must exist (see ``dune pkg lock``).

   In a portable lock directory, the conditions on each depext entry are
   evaluated against the current platform to determine which system package
   names apply.

   ``--context CONTEXT``
      Select which build context's dependencies to query. Defaults to
      ``default``.

   .. code:: console

      $ dune show depexts
      gnupg
      unzip

Platform Variables
------------------

Depext filter conditions may reference the following platform variables. The
names below use the opam convention (hyphens); in lock file conditions the
corresponding Dune variable names use underscores.

``os``
   Operating system (e.g., ``linux``, ``macos``, ``win32``).

   Override with the environment variable ``DUNE_CONFIG__OS``.

``os-family``
   OS family (e.g., ``debian``, ``bsd``, ``windows``).

   Override with the environment variable ``DUNE_CONFIG__OS_FAMILY``.

``os-distribution``
   OS distribution (e.g., ``ubuntu``, ``archlinux``, ``centos``,
   ``homebrew``).

   Override with the environment variable ``DUNE_CONFIG__OS_DISTRIBUTION``.

``os-version``
   OS version string.

   Override with the environment variable ``DUNE_CONFIG__OS_VERSION``.

``arch``
   CPU architecture (e.g., ``x86_64``, ``arm64``).

   Override with the environment variable ``DUNE_CONFIG__ARCH``.

``sys-ocaml-version``
   OCaml compiler version string (as reported by ``ocamlc -vnum``).

   Override with the environment variable ``DUNE_CONFIG__SYS_OCAML_VERSION``.

In a standard lock directory, Dune evaluates these variables at solve time
(during ``dune pkg lock``) and records only the matching depexts. In a portable
lock directory, the conditions are stored and these variables are evaluated at
build time instead. Dune detects platform values using system tools such as
``uname`` and ``/etc/os-release``. The environment variable overrides allow
testing depext resolution for a different platform without changing the host
system.

If a depext condition references an undefined variable, that depext entry is
silently excluded from the results.

Build Failure Hints
-------------------

When a locked package fails to build and that package has associated depexts,
Dune prints a hint listing that package's depexts after the error message (unlike
``dune show depexts``, which aggregates depexts across all locked packages):

.. code:: console

   Error: Program pkg-config not found in the tree or in PATH
    (context: default)
   Hint: You may want to verify the following depexts are installed:
   - gnupg
   - unzip

This hint is informational. Dune does not install or manage system packages.

.. seealso::

   :doc:`/explanation/package-management`
      Conceptual overview of Dune's package management.

   :doc:`/reference/packages`
      Package definitions and opam file generation.

   :doc:`/reference/dune-workspace/lock_dir`
      Configuration of the lock directory.

.. TODO: link to explanation section on depexts once added to
   doc/explanation/package-management.md (see #13657)

.. TODO: link to tutorial on depexts once added to
   doc/tutorials/dune-package-management/ (see #13657)

.. TODO: link to how-to guide on handling external dependencies once
   doc/howto/handle-external-dependencies.md is created (see #13657)

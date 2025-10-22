
Terminology
===========

.. glossary::
   root
     The top-most directory in a GitHub repo, workspace, and project,
     differentiated by variables such as ``%{workspace_root}`` and
     ``%{project_root}``. Dune builds things from this directory. It knows how to
     build targets that are descendants of the root. Anything outside of the tree
     starting from the root is considered part of the :term:`installed world`.
     Refer to :ref:`finding-root` to learn how the workspace root is determined.

   workspace
     The subtree starting from each root. It can contain any number of projects
     that will be built simultaneously by Dune, and it must contain a
     ``dune-workspace`` file.

   project
     A collection of source files that must include a ``dune-project`` file. It
     may also contain one or more packages. A project consists in a hierarchy
     of directories. Every directory (at the root, or a subdirectory) can
     contain a ``dune`` file that contains instructions to build files in that
     directory. Projects can be shared between different applications.

   package
     A set of libraries and executables that opam builds and installs as one.

   installed world
     Anything outside of the workspace. Dune doesn't know how to build things
     in the installed world.

   installation
     The action of copying build artifacts or other files from the
     ``<root>/_build`` directory to the :term:`installed world`.

   scope
     Defined by any directory that contains at least one `<package>.opam` file.
     Typically, every project defines a single scope that is a subtree starting
     from this directory. Moreover, scopes are separate from your project's
     dependencies. The scope also determines where private items are visible.
     Private items include libraries or binaries that will not be installed.
     See :doc:`/core-concepts/scopes` for more details.

   build context
     A specific configuration written in a
     :doc:`/reference/dune-workspace/index` file, which has a
     corresponding subdirectory in the ``<root>/_build`` directory. It contains
     all the workspace's build artifacts. Without this specific configuration
     from the user, there is always a ``default`` build context that
     corresponds to the executed Dune environment.

   build context root
     The root of a build context named ``foo`` is ``<root>/_build/<foo>``.

   build target
     Specified on the command line, e.g., ``dune build <target_path.exe>``. All
     targets that Dune knows how to build live in the ``_build`` directory.

   alias
     A build target that doesn't produce any file and has configurable
     dependencies. Targets starting with ``@`` on the command line are
     interpreted as aliases (e.g., ``dune build @src/runtest``). Aliases are
     per-directory. See :doc:`/reference/aliases`.

   environment
     Determines the default values of various parameters, such as the
     compilation flags. In Dune, each directory has an environment attached to
     it. Inside a scope, each directory inherits the environment from its
     parent. At the root of every scope, a default environment is used. At any
     point, the environment can be altered using an
     :doc:`/reference/dune/env` stanza.

   build profile
     A global setting that influences various defaults. It can be set from the
     command line using ``--profile <profile>`` or from ``dune-workspace``
     files. The following profiles are standard:

     -  ``release`` which is the profile used for opam releases
     -  ``dev`` which is the default profile when none is set explicitly, it has
        stricter warnings than the ``release`` one

   dialect
     An alternative frontend to OCaml (such as ReasonML). It is described
     by a pair of file extensions, one corresponding to interfaces and one to
     implementations. It can use the standard OCaml syntax, or it can specify an
     action to convert from a custom syntax to a binary OCaml abstract syntax
     tree. It can also specify a custom formatter.

   placeholder substitution
     A build step in which placeholders such as ``%%VERSION%%`` in source files
     are replaced by concrete values such as ``1.2.3``. It is performed by
     :ref:`dune-subst` for development versions and dune-release_ for
     releases.

   stanza
     A fragment of a file interpreted by Dune, that will appear as a
     s-expression at the top-level of a file. For example, the
     :doc:`/reference/dune/library` stanza describes a library. This can be
     either a generic term ("the library stanza") or it can refer to a
     particular instance in a file ("the executable stanza in ``bin/dune``").

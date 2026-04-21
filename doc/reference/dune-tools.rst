dune tools
==========

.. warning::

   The ``dune tools`` command group is **experimental**. Its subcommands,
   flags, and behavior may change in future versions of Dune without notice.
   The internal APIs that power these commands are also subject to change.

   If you encounter issues, please `open an issue
   <https://github.com/ocaml/dune/issues>`_ in the Dune issue tracker.

``dune tools`` is a command group for managing developer tools. Developer tools
are programs that are useful for working on a project's source code but are not
required for building or deploying the project itself. Examples include
`ocamlformat <https://github.com/ocaml-ppx/ocamlformat>`_ for formatting your
source code, `ocaml-lsp-server <https://github.com/ocaml/ocaml-lsp>`_ which
provides a Language Server Protocol implementation for OCaml, and `utop
<https://github.com/ocaml-community/utop>`_ an improved toplevel (REPL).

Dune can automatically resolve, lock, build, and run these tools using package
management. Each tool is managed in its own lock directory separate from the
project.

Supported Tools
---------------

The following developer tools are currently supported:

.. list-table::
   :header-rows: 1
   :widths: 20 20 20

   * - Tool
     - Executable
     - Package
   * - ocamlformat
     - ``ocamlformat``
     - ``ocamlformat``
   * - odoc
     - ``odoc``
     - ``odoc``
   * - ocaml-lsp-server
     - ``ocamllsp``
     - ``ocaml-lsp-server``
   * - utop
     - ``utop``
     - ``utop``
   * - ocamlearlybird
     - ``ocamlearlybird``
     - ``earlybird``
   * - odig
     - ``odig``
     - ``odig``
   * - opam-publish
     - ``opam-publish``
     - ``opam-publish``
   * - dune-release
     - ``dune-release``
     - ``dune-release``
   * - ocaml-index
     - ``ocaml-index``
     - ``ocaml-index``
   * - merlin
     - ``ocamlmerlin``
     - ``merlin``

.. note::

   ``dune tools exec`` supports all tools above except ``odoc`` and ``utop``.
   The ``install`` and ``which`` subcommands support all listed tools.

   ``odoc`` and ``utop`` have their own dedicated commands to execute:

   - ``utop`` is run via ``dune utop [DIR]``, which discovers libraries in the
     given directory (defaults to working directory), configures library search
     paths, and generates a custom ``findlib.conf`` so that utop can find
     dependencies within ``_build``. ``utop`` needs to be installed via ``opam``
     or ``dune tools install utop`` before invoking this command.
   - ``odoc`` is invoked via ``dune ocaml doc``, which builds documentation
     using the ``@doc`` alias. As with ``utop``, ``odoc`` must be installed
     before use.

Subcommands
-----------

``dune tools exec``
~~~~~~~~~~~~~~~~~~~

.. code-block:: console

   $ dune tools exec <tool> [-- <args>...]

Run a developer tool. Dune will automatically resolve, lock, and build the
tool if it has not been installed yet, then execute it. This is intended to be
called by text editors and other tooling that needs to invoke a dev tool.

All positional arguments are forwarded to the tool's executable.
.. code-block:: console

   $ dune tools exec ocamlformat
   $ dune tools exec --help

``dune tools install``
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: console

   $ dune tools install <tool>

Install a developer tool without running it. Dune will resolve dependencies,
create a separate lock directory, and build the tool. This is also useful for
pre-installing tools so that later ``exec`` or ``which`` calls are fast.

.. code-block:: console

   $ dune tools install ocamlformat
   $ dune tools install ocamllsp

``dune tools which``
~~~~~~~~~~~~~~~~~~~~

.. code-block:: console

   $ dune tools which <tool> [--allow-not-installed]

Print the path to a developer tool's executable. By default, this errors if
the tool has not been installed yet.

Flags:

* ``--allow-not-installed``
   Print the path where the tool would be installed, even if it does not
   exist yet.

.. code-block:: console

   $ dune tools which ocamlformat
   _build/.dev-tools.locks/ocamlformat/ocamlformat/target/bin/ocamlformat

   $ dune tools which merlin --allow-not-installed
   _build/.dev-tools.locks/merlin/merlin/target/bin/ocamlmerlin

``dune tools env``
~~~~~~~~~~~~~~~~~~

.. code-block:: console

   $ dune tools env [--fish]

Print a shell command that, when evaluated, adds all dev tool binary
directories to your ``PATH``. This allows running dev tools directly as
commands without going through ``dune tools exec``.

Flags:

* ``--fish``
   Emit a ``fish_add_path`` command suitable for the fish shell instead of
   the default POSIX ``export PATH=...`` form.


For POSIX-compatible shells (bash, zsh, etc.):

.. code-block:: console

   $ eval $(dune tools env)

For the fish shell:

.. code-block:: console

   $ eval (dune tools env --fish)

.. note::

   The design of ``dune tools`` is still being fleshed out and may be
   reworked. See `dune#13457 <https://github.com/ocaml/dune/pull/13457>`_ for
   ongoing discussion.

See Also
--------

- :doc:`/howto/customize-dev-tools-lock-directories` for customizing the lock
  directory configuration of developer tools.

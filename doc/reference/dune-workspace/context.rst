context
-------
.. CR sudha247: The introductory description of context can be improved to
.. better convey what it does.

The ``(context ...)`` stanza allows you to declare multiple build contexts in
the workspace. Each context has its own configuration, and multiple contexts can
be built side by side. By default Dune uses a single context called ``default``
whose build directory artifacts appear in ``_build/default/``. There are two
different kinds of context declarations: ``(default ...)`` for the regular kind
of build context, or ``(opam ...)`` to use an opam switch.

.. note::

   Despite its name, the ``(default ...)`` field of the ``context`` stanza
   specifies what we call the **regular context**, which is named this way to
   avoid confusion with the default context name.

.. code:: dune

    (context
     (default
      <optional-fields>))

.. code:: dune

    (context
     (opam
      (switch <opam-switch-name>)
      <optional-fields>))

``<optional-fields>`` common to both context types are:

- ``(name <name>)`` is the name of the subdirectory within ``_build`` where this
  context's build artifacts will be stored. If omitted, the context name is
  ``default`` for the default context, and the switch name for the opam context.

- ``(merlin)`` instructs Dune to use this build context for Merlin.

- ``(generate_merlin_rules)`` instructs Dune to generate Merlin rules for this
  context, even if it is not the one selected via ``(merlin)``.

- ``(profile <profile>)`` sets the profile for this :term:`build context`. This
  takes precedence over the command-line option ``--profile``.

- ``(env <env>)`` sets the environment for a particular context. This takes
  higher precedence over the root ``env`` stanza in the workspace file. This
  field has the same options as the :doc:`/reference/dune/env` stanza.

- ``(toolchain <findlib_toolchain>)`` sets a ``findlib`` toolchain for the
  context.

- ``(host <host_context>)`` specifies a different context to build binaries that
  are meant to be executed on the host machine, such as preprocessors. See
  :ref:`cross-compilation` for more information. This is mutually exclusive with
  the ``(targets ...)`` field.

- ``(targets <targets>)`` specifies target names for cross compilation targets.
  See :ref:`cross-compilation` for more information. This is mutually exclusive
  with the ``(host ...)`` field.

- ``(paths (<var1> <val1>) .. (<varN> <valN>))`` allows you to set the value of
  any ``PATH``-like variables in this context. If ``PATH`` itself is modified in
  this way, its value will be used to resolve workspace binaries, including
  finding the compiler and related tools. These variables will also be passed as
  part of the environment to any program launched by Dune. For each variable,
  the value is specified using the :doc:`/reference/ordered-set-language`.
  Relative paths are interpreted with respect to the workspace root. See
  :ref:`finding-root`.

- ``(fdo <target_exe>)`` builds this context with feedback-directed
  optimizations. It requires `OCamlFDO
  <https://github.com/gretay-js/ocamlfdo>`__. ``<target_exe>`` is a path
  interpreted relative to the workspace root (see :ref:`finding-root`).
  ``<target_exe>`` specifies which executable to optimize. Users should define a
  different context for each target executable built with FDO. The context name
  is derived automatically from the default name and ``<target-exe>``, unless
  explicitly specified using the ``(name ...)`` field. For example, if
  ``<target_exe>`` is *src/foo.exe* in a default context, then the name of the
  context is *default-fdo-foo* and the filename that contains execution counters
  is *src/fdo.exe.fdo-profile*.  This feature is **experimental** and no
  backward compatibility is implied.

- ``(instrument_with <instrumentation_backend>)`` turns on instrumentation for
  the context. See :doc:`/instrumentation` for more information.

- ``(disable_dynamically_linked_foreign_archives <bool>)`` disables Dune's
  default behavior of building and installing dynamically-linked foreign
  archives (e.g., ``dll*.so``), so bytecode executables are built with all
  foreign archives statically linked into the runtime system.

``<optional-fields>`` specific to ``(context (default ...))`` are:

- ``(lock_dir <path>)`` specifies the lock directory that will be used for
   building this context (if any). If no lock directory is specified,
   ``dune.lock`` will be used. See the :doc:`/reference/dune-workspace/lock_dir`
   stanza for lock directory configuration options.

``<optional-fields>`` specific to ``(context (opam ...))`` are:

- ``(root <opam-root>)`` is the opam root. By default, it will take the opam
   root defined by the environment in which ``dune`` is run, which is usually
   ``~/.opam``.

Merlin reads compilation artifacts, and it can only read the compilation
artifacts of a single context. Usually, you should use the artifacts from the
``default`` context, and if you have the ``(context default)`` stanza in your
``dune-workspace`` file, that is the one Dune will use.

For rare cases where this is not what you want, you can force Dune to use a
different build context for Merlin by adding the field ``(merlin)`` to this
context.

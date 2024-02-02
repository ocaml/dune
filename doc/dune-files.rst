****************
Stanza Reference
****************

.. TODO(diataxis)

   Recycle this content into:

   - :doc:`reference/stanzas`
   - :doc:`reference/files`

.. _dune-project:

dune-project
============

See :doc:`reference/files/dune-project/index`.

dune
====

See :doc:`reference/files/dune/index`.

.. _dune-workspace:

dune-workspace
==============

By default, a workspace has only one build context named ``default`` which
corresponds to the environment, in which ``dune`` is run. You can define more
contexts by writing a ``dune-workspace`` file.

You can point Dune to an explicit ``dune-workspace`` file with the
``--workspace`` option. For instance, it's good practice to write a
``dune-workspace.dev`` in your project with all the OCaml versions your projects
support, so developers can test that the code builds with all OCaml versions by
simply running:

.. code:: console

    $ dune build --workspace dune-workspace.dev @all @runtest

The ``dune-workspace`` file uses the S-expression syntax. This is what a typical
``dune-workspace`` file looks like:

.. code:: dune

    (lang dune 3.14)
    (context (opam (switch 4.07.1)))
    (context (opam (switch 4.08.1)))
    (context (opam (switch 4.11.1)))

The rest of this section describe the stanzas available.

Note that an empty ``dune-workspace`` file is interpreted the same as one
containing exactly:

.. code:: dune

    (lang dune 3.2)
    (context default)

This allows you to use an empty ``dune-workspace`` file to mark the root of your
project.

env
---

The ``env`` stanza can be used to set the base environment for all contexts in
this workspace. This environment has the lowest precedence of all other ``env``
stanzas. The syntax for this stanza is the same as Dune's :ref:`dune-env`
stanza.

``config`` stanzas
------------------

Starting in Dune 3.0, any of the stanzas from the :ref:`config` file can be used
in the ``dune-workspace`` file. In this case, the configuration stanza will only
affect the current workspace.

context
-------

The ``(context ...)`` stanza declares a build context. The argument can be
either ``default`` or ``(default)`` for the default build context, or it can be
the description of an opam switch, as follows:

.. code:: dune

    (context (opam (switch <opam-switch-name>)
                   <optional-fields>))

``<optional-fields>`` are:

-  ``(name <name>)`` is the subdirectory's name for ``_build``, where this
   build's context artifacts will be stored.

-  ``(root <opam-root>)`` is the opam root. By default, it will take the opam
   root defined by the environment in which ``dune`` is run, which is usually
   ``~/.opam``.

- ``(merlin)`` instructs Dune to use this build context for Merlin.

- ``(profile <profile>)`` sets a different profile for a :term:`build context`. This has
  precedence over the command-line option ``--profile``.

- ``(env <env>)`` sets the environment for a particular context. This is of
  higher precedence than the root ``env`` stanza in the workspace file. This
  field has the same options as the :ref:`dune-env` stanza.

- ``(toolchain <findlib_toolchain>)`` sets a ``findlib`` toolchain for the
  context.

- ``(host <host_context>)`` chooses a different context to build binaries that
  are meant to be executed on the host machine, such as preprocessors.

- ``(paths (<var1> <val1>) .. (<varN> <valN>))`` allows you to set the value of
  any ``PATH``-like variables in this context. If ``PATH`` itself is modified in
  this way, its value will be used to resolve workspace binaries, including
  finding the compiler and related tools. These variables will also be passed as
  part of the environment to any program launched by Dune. For each variable,
  the value is specified using the :doc:`reference/ordered-set-language`.
  Relative paths are interpreted with respect to the workspace root. See
  :ref:`finding-root`.

- ``(fdo <target_exe>)`` builds this context with feedback-direct optimizations.
  It requires `OCamlFDO <https://github.com/gretay-js/ocamlfdo>`__.
  ``<target_exe>`` is a path-interpreted relative to the workspace root (see
  :ref:`finding-root`). ``<target_exe>`` specifies which executable to optimize.
  Users should define a different context for each target executable built with
  FDO. The context name is derived automatically from the default name and
  ``<target-exe>``, unless explicitly specified using the ``(name ...)`` field.
  For example, if ``<target_exe>`` is *src/foo.exe* in a default context, then
  the name of the context is *default-fdo-foo* and the filename that contains
  execution counters is *src/fdo.exe.fdo-profile*.  This feature is
  **experimental** and no backwards compatibility is implied.

- By default, Dune builds and installs dynamically-linked foreign archives
  (usually named ``dll*.so``). It's possible to disable this by setting by
  including ``(disable_dynamically_linked_foreign_archives true)`` in the
  workspace file, so bytecode executables will be built with all foreign
  archives statically linked into the runtime system.


Both ``(default ...)`` and ``(opam ...)`` accept a ``targets`` field in order to
setup cross compilation. See :ref:`cross-compilation` for more information.

Merlin reads compilation artifacts, and it can only read the compilation
artifacts of a single context. Usually, you should use the artifacts from the
``default`` context, and if you have the ``(context default)`` stanza in your
``dune-workspace`` file, that is the one Dune will use.

For rare cases where this is not what you want, you can force Dune to use a
different build contexts for Merlin by adding the field ``(merlin)`` to this
context.

profile
-------

The build profile can be selected in the ``dune-workspace`` file by write a
``(profile ...)`` stanza. For instance:

.. code:: dune

    (profile release)

Note that the command line option ``--profile`` has precedence over this stanza.

.. _config:

config
======

See :doc:`reference/files/config/index`.

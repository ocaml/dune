.. _workspace-configuration:

************************
``dune-workspace`` files
************************

By default, a workspace has only one build context named ``default`` which
correspond to the environment in which ``dune`` is run. You can define more
contexts by writing a ``dune-workspace`` file.

You can point ``dune`` to an explicit ``dune-workspace`` file with the
``--workspace`` option. For instance it is good practice to write a
``dune-workspace.dev`` in your project with all the version of OCaml your
projects support. This way developers can tests that the code builds with all
version of OCaml by simply running:

.. code:: bash

    $ dune build --workspace dune-workspace.dev @all @runtest

The ``dune-workspace`` file uses the S-expression syntax. This is what
a typical ``dune-workspace`` file looks like:

.. code:: scheme

    (lang dune 1.0)
    (context (opam (switch 4.02.3)))
    (context (opam (switch 4.03.0)))
    (context (opam (switch 4.04.0)))

The rest of this section describe the stanzas available.

Note that an empty ``dune-workspace`` file is interpreted the same as one
containing exactly:

.. code:: scheme

    (lang dune 1.0)
    (context default)

This allows you to use an empty ``dune-workspace`` file to mark the root of your
project.

profile
~~~~~~~

The build profile can be selected in the ``dune-workspace`` file by write a
``(profile ...)`` stanza. For instance:

.. code:: scheme

    (profile release)

Note that the command line option ``--profile`` has precedence over this stanza.

env
~~~

The ``env`` stanza can be used to set the base environment for all contexts in
this workspace. This environment has the lowest precedence of all other ``env``
stanzas. The syntax for this stanza is the same dune's :ref:`dune-env` stanza.

context
~~~~~~~

The ``(context ...)`` stanza declares a build context. The argument
can be either ``default`` or ``(default)`` for the default build
context or can be the description of an opam switch, as follows:

.. code:: scheme

    (context (opam (switch <opam-switch-name>)
                   <optional-fields>))

``<optional-fields>`` are:

-  ``(name <name>)`` is the name of the subdirectory of ``_build``
   where the artifacts for this build context will be stored

-  ``(root <opam-root>)`` is the opam root. By default it will take
   the opam root defined by the environment in which ``dune`` is
   run which is usually ``~/.opam``

- ``(merlin)`` instructs dune to use this build context for
  merlin

- ``(profile <profile>)`` to set a different profile for a build
  context. This has precedence over the command line option
  ``--profile``

- ``(env <env>)`` to set the environment for a particular context. This is of
  higher precedence than the root ``env`` stanza in the workspace file. This
  field the same options as the :ref:`dune-env` stanza.

- ``(toolchain <findlib_coolchain>)`` set findlib toolchain for the context.

- ``(host <host_context>)`` choose a different context to build binaries that
  are meant to be executed on the host machine, such as preprocessors.

- ``(paths (<var1> <val1>) .. (<varN> <valN>))`` allows to set the value of any
  ``PATH``-like variables in this context. If ``PATH`` itself is modified in
  this way, its value will be used to resolve binaries in the workspace,
  including finding the compiler and related tools. These variables will also be
  passed as part of the environment to any program launched by ``dune``. For
  each variable, the value is specified using the :ref:`ordered-set-language`.
  Relative paths are interpreted with respect to the workspace root, see
  :ref:`finding-root`.

Both ``(default ...)`` and ``(opam ...)`` accept a ``targets`` field in order to
setup cross compilation. See :ref:`advanced-cross-compilation` for more
information.

Merlin reads compilation artifacts and it can only read the compilation
artifacts of a single context. Usually, you should use the artifacts from the
``default`` context, and if you have the ``(context default)`` stanza in your
``dune-workspace`` file, that is the one dune will use.

For rare cases where this is not what you want, you can force dune to use a
different build contexts for merlin by adding the field ``(merlin)`` to this
context.

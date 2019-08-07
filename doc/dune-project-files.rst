**********************
``dune-project`` files
**********************

These files are used to mark the root of projects as well as define project-wide
parameters. These files are required to have a ``lang`` which controls the names
and contents of all configuration files read by Dune. The ``lang`` stanza looks
like:

.. code:: scheme

          (lang dune 1.0)

Additionally, they can contains the following stanzas.

name
----

Sets the name of the project. This is used by :ref:`dune subst <dune-subst>`
and error messages.

.. code:: scheme

    (name <name>)

version
-------

Sets the version of the project:

.. code:: scheme

    (version <version>)

implicit_transitive_deps
------------------------

Enables or disables :ref:`implicit-transitive-deps`:

.. code:: scheme

    (implicit_transitive_deps <bool>)

wrapped_executables
-------------------

Enables or disables :ref:`wrapped-executables`:

.. code:: scheme

    (wrapped_executables <bool>)

explicit_js_mode
----------------

Enables :ref:`explicit-js-mode`:

.. code:: scheme

    (explicit_js_mode)

dialect
-------

Defines :ref:`dialects-main` for this project:

.. code:: scheme

    (dialect
     (name <name>)
     (implementation
      (extension <string>)
      <optional fields>)
     (interface
      (extension <string>)
      <optional fields>))

``<name>`` is the name of the dialect being defined. It must be unique in a
given project.

``(extension <string>)`` specifies the file extension used for this dialect, for
interfaces and implementations. The extension string must not contain any dots,
and be unique in a given project.

``<optional fields>`` are:

- ``(preprocess <action>)`` is the action to run to produce a valid OCaml
  abstract syntax tree. It is expected to read the file given in the variable
  named ``input-file`` and output a *binary* abstract syntax tree on its
  standard output. See :ref:`preprocessing-actions` for more information.

  If the field is not present, it is assumed that the corresponding source code
  is already valid OCaml code and can be passed to the OCaml compiler as-is.

- ``(format <action>)`` is the action to run to format source code for this
  dialect. The action is expected to read the file given in the variable named
  ``input-file`` and output the formatted source code on its standard
  output. For more information. See :ref:`formatting-main` for more information.

  If the field is not present, then if ``(preprocess <action>)`` is not present
  (so that the dialect consists of valid OCaml code), then by default the
  dialect will be formatted as any other OCaml code. Otherwise no special
  formatting will be done.

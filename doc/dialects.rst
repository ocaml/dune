.. _dialects-main:

********
Dialects
********

A dialect is an alternative frontend to OCaml (such as ReasonML). It is
described by a pair of file extensions, one corresponding to interfaces and one
to implementations.

The extensions are unique among all dialects of a given project, so that a given
extension can be mapped back to the corresponding dialect.

A dialect can use the standard OCaml syntax or it can specify an action to
convert from a custom syntax to a binary OCaml abstract syntax tree.

Similarly, a dialect can specify a custom formatter to implement the ``@fmt``
alias, see :ref:`formatting-main`.

When not using a custom syntax or formatting action, a dialect is nothing but a
way to specify custom file extensions for OCaml code.

Defining a dialect
==================

A dialect can be defined by adding the following to the ``dune-project`` file:

.. code::

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

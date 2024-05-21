dialect
-------

.. describe:: (dialect ...)

   Declare a new :term:`dialect`.

   .. describe:: (name <name>)

      The name of the dialect being defined. It must be unique in a given
      project.

      This field is required.

   .. describe:: (implementation ...)

      Details related to the implementation files (corresponding to `*.ml`).

      .. versionchanged:: 3.9 This field is made optional.

      .. describe:: (extension <string>)

         Specify the file extension used for this dialect.

         The extension string must not start with a period and be unique in a
         given project (so that a given extension can be mapped back to a
         corresponding dialect). In Dune 3.9 and later, the extension string may
         contain periods (e.g., `cppo.ml`).

         This field is required.

      .. describe:: (preprocess <action>)

         Run `<action>` to produce a valid OCaml abstract syntax tree.

         This action is expected to read the file given in the variable named
         ``%{input-file}`` and output a *binary* abstract syntax tree on its
         standard output.

         If the field is not present, it is assumed that the corresponding
         source code is already valid OCaml code and can be passed to the OCaml
         compiler as-is.

         .. seealso:: :ref:`preprocessing-actions`

      .. describe:: (format <action>)

         Run `<action>` to format source code for this dialect.

         The action is expected to read the file given in the variable named
         ``%{input-file}`` and output the formatted source code on its standard
         output.

         If the field is not present, the behavior depends on the presence of
         ``(preprocess)``: if it is also not present (that is, the dialect
         consists of valid OCaml code), then the dialect will be formatted as
         any other OCaml code. Otherwise no special formatting will be done.

         .. seealso:: :doc:`/howto/formatting`

   .. describe:: (interface ...)

      Details related to the interface files (corresponding to `*.mli`).

      This field supports the same sub-fields as ``implementation``.

      .. versionchanged:: 3.9 This field is made optional.

   .. describe:: (merlin_reader <program> <args>...)

      Configure Merlin to use `<program> <args>...` as READER. Merlin's READER
      is a mechanism to extend Merlin to support OCaml dialects by providing
      a program that transforms a dialect AST into an OCaml AST.

      .. seealso:: `merlin/src/extend/extend_protocol.ml <https://github.com/ocaml/merlin/blob/4.14-502_preview2/src/extend/extend_protocol.ml>`_ for the protocol specification.

      This field is optional.

      .. versionadded:: 3.16


Default dialects
----------------

Dune ships with two dialects pre-configured and enabled:

* ``ocaml`` for the default OCaml syntax which consumes `.ml` and `.mli` files
  and uses ``ocamlformat`` for formatting.
* ``reason`` for the Reason syntax and enabled in `.re`/`.rei` files. ``refmt``
  is used for formatting.

A third dialect, ``rescript``, is added when Melange support (see :doc:`/melange`)
is enabled in the project.

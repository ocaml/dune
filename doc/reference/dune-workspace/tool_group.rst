tool_group
==========

.. warning::

   This stanza is a work in progress. It is parsed and validated, but tools
   declared with it are not yet locked, built, or runnable.

Declares developer tools that are managed by Dune's package management but are
not dependencies of the project, such as ``ocamlformat`` or
``ocaml-lsp-server``.

.. describe:: (tool_group ...)

   .. versionadded:: 3.25

   .. describe:: (name <string>)

      Optional. Two groups may not share a name.

   .. describe:: (tools <dep-specification> ...)

      The packages providing the tools, in the
      :token:`~pkg-dep:dep_specification` format used by ``depends``. At least
      one is required. A package may be declared at most once per inherited
      context, and at most once among groups that do not inherit a context.

   .. describe:: (lock_dir ...)

      Required. Either the fields of the :doc:`lock_dir` stanza, or:

      .. describe:: (inherit <context>)

         Build the tools on top of the packages of the named
         :doc:`context`. Cannot be combined with other ``lock_dir`` fields.

      .. describe:: (shared_packages <name> ...)

         Reuse only these packages, and what they depend on, from the inherited
         context. Requires ``inherit``. Defaults to all of them.

Example:

.. code:: dune

   (tool_group
    (tools ocamlformat)
    (lock_dir))

   (tool_group
    (tools (ocaml-lsp-server (>= 1.27.0)) utop)
    (lock_dir (inherit default)))

tool_group
==========

.. warning::

   This stanza is a work in progress. It is parsed and validated, but tools
   declared with it are not yet locked, built, or runnable.

Declares developer tools that are managed by Dune's package management but are
not dependencies of the project, such as ``ocamlformat`` or
``ocaml-lsp-server``.

The stanza is unreleased. To enable it, add ``(using unreleased 0.1)``
:doc:`extension </reference/dune-project/using>` to your ``dune-workspace``
file. It may change without notice.

.. describe:: (tool_group ...)

   .. describe:: (name <string>)

      Optional. Two groups may not share a name.

   .. describe:: (tools <dep-specification> ...)

      The packages providing the tools, in the
      :token:`~pkg-dep:dep_specification` format used by ``depends``. At least
      one is required. A package may be declared at most once per inherited
      context, and at most once among groups that do not inherit a context.

   Exactly one of ``lock_dir`` or ``inherit`` is required.

   .. describe:: (lock_dir ...)

      Solve the tools in isolation. Accepts the fields of the :doc:`lock_dir`
      stanza.

   .. describe:: (inherit ...)

      Build the tools on top of the packages of an existing context.

      .. describe:: (context <name>)

         Required. The :doc:`context` whose lock directory the tools are
         solved on top of. It must be a default context, not an opam one.

      .. describe:: (shared_packages <name> ...)

         Optional. Reuse only these packages, and what they depend on, from
         the inherited context. Defaults to all of them.

Example:

.. code:: dune

   (tool_group
    (tools ocamlformat)
    (lock_dir))

   (tool_group
    (tools (ocaml-lsp-server (>= 1.27.0)) utop)
    (inherit (context default)))

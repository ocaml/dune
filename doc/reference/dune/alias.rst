.. highlight:: dune

alias
-----

.. describe:: (alias ...)

   Add dependencies to an alias.

   Aliases do not need to be explicitly created, adding to a new name will
   "create" an alias.
   An alias with name ``x`` can be built by running ``dune build @x``.
   See :doc:`/reference/aliases`.

   The common use of the ``alias`` stanza is make an alias depend on other ones::

       (alias
        (name runtest)
        (deps
         (alias test-unit)
         (alias test-integration)))

   .. warning::

      In previous versions of the dune language, it was also possible to specify
      an action to run to construct the alias. Please use a :doc:`rule` stanza
      with the ``alias`` field instead.

   This stanza supports the following fields:

   .. describe:: (name <name>)

      An alias name.

      Attaching dependencies to ``(name x)`` will ensure they are built by
      ``dune build @x``.

      This field is required.

   .. describe:: (deps <deps-conf list)

      Specifies the dependencies of the alias.

      See :doc:`/concepts/dependency-spec` for more details.

      This field is required.

   .. describe:: (enabled_if <blang expression>)

      Specifies the Boolean condition that must be true for the tests to run.

      The condition is specified using the :doc:`/reference/boolean-language`, and
      the field allows for :doc:`/concepts/variables` to appear in the expressions.

   .. describe:: (action <action>)

      .. versionremoved :: 2.0 use :doc:`rule` with the ``alias`` field instead.

      An :doc:`action </reference/actions/index>` for constructing the alias.

   .. describe:: (package <name>)

      Indicates that this alias stanza is part of package ``<name>`` and should be
      filtered out if ``<name>`` is filtered out from the command line, either with
      ``--only-packages <pkgs>`` or ``-p <pkgs>``.

   .. describe:: (locks (<lock-names>))

      Specifies that the action must be run while holding the following locks. See
      :doc:`/concepts/locks` for more details.

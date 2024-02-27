#######
 alias
#######

The ``alias`` stanza adds dependencies to an alias or specifies an
action to run to construct the alias.

The syntax is as follows:

.. code:: dune

   (alias
    (name    <alias-name>)
    (deps    <deps-conf list>)
    <optional-fields>)

``<name>`` is an alias name such as ``runtest``.

.. _alias-fields:

``<deps-conf list>`` specifies the dependencies of the alias. See
:doc:`/concepts/dependency-spec` for more details.

``<optional-fields>`` are:

-  ``<action>``, an action for constructing the alias. See
   :doc:`/reference/actions/index` for more details. Note that this is
   removed in Dune 2.0, so users must port their code to use the
   ``rule`` stanza with the ``alias`` field instead.

-  ``(package <name>)`` indicates that this alias stanza is part of
   package ``<name>`` and should be filtered out if ``<name>`` is
   filtered out from the command line, either with ``--only-packages
   <pkgs>`` or ``-p <pkgs>``.

-  ``(locks (<lock-names>))`` specifies that the action must be run
   while holding the following locks. See :doc:`/concepts/locks` for
   more details.

-  ``(enabled_if <blang expression>)`` specifies the Boolean condition
   that must be true for the tests to run. The condition is specified
   using the :doc:`/reference/boolean-language`, and the field allows
   for :doc:`/concepts/variables` to appear in the expressions.

The typical use of the ``alias`` stanza is to define tests:

.. code:: dune

   (rule
    (alias   runtest)
    (action (run %{exe:my-test-program.exe} blah)))

See the section about :ref:`running-tests` for details.

Please note: if your project contains several packages, and you run the
tests from the opam file using a ``build-test`` field, all your
``runtest`` alias stanzas should have a ``(package ...)`` field in order
to partition the set of tests.

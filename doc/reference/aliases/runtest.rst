@runtest
========

Actions that run tests are attached to this alias. For example this convention
is used by the ``(test)`` stanza.

Building ``@runtest`` directly uses normal :doc:`alias </reference/aliases>`
target semantics. In a workspace with multiple contexts, an unqualified alias
target is built in every configured context.

The ``dune runtest`` command, also available as ``dune test``, has different
command-line behavior: it accepts test path arguments and selects a build
context for source paths. See :ref:`writing-and-running-tests-running-tests`
for details.

.. seealso:: :doc:`/tests`

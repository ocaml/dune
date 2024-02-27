#########
 include
#########

The ``include`` stanza allows including the contents of another file in
the current ``dune`` file. Currently, the included file cannot be
generated and must be present in the source tree. This feature is
intended for use in conjunction with promotion, when parts of a ``dune``
file are to be generated.

For instance:

.. code:: dune

   (include dune.inc)

   (rule (with-stdout-to dune.inc.gen (run ./gen-dune.exe)))

   (rule
    (alias  runtest)
    (action (diff dune.inc dune.inc.gen)))

With this ``dune`` file, running Dune as follows will replace the
``dune.inc`` file in the source tree by the generated one:

.. code:: console

   $ dune build @runtest --auto-promote

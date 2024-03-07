pipe-<outputs>
--------------

.. highlight:: dune

.. describe:: (pipe-<outputs> <DSL> <DSL> <DSL>...)

   .. versionadded:: 2.7

   Execute several actions (at least two) in sequence, filtering the
   ``<outputs>`` of the first command through the other command, piping the
   standard output of each one into the input of the next.

   Example::

      (pipe-stdout
       (run ./list-tests.exe)
       (run ./exec-tests.exe))

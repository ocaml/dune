with-<outputs>-to
-----------------

.. highlight:: dune

.. describe:: (with-<outputs>-to <file> <DSL>)

   Redirect the output to a file, where ``<outputs>`` is one of: ``stdout``,
   ``stderr`` or ``outputs`` (for both ``stdout`` and ``stderr``).

   Example::

     (with-stdout-to conf.txt
      (run ./get-conf.exe))

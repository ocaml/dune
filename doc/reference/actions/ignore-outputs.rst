ignore-<outputs>
----------------

.. highlight:: dune

.. describe:: (ignore-<outputs> <DSL>)

   Ignore the output, where ``<outputs>`` is one of: ``stdout``, ``stderr``, or
   ``outputs``.

   Example::

     (ignore-stderr
      (run ./get-conf.exe))

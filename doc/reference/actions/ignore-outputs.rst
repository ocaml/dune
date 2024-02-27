ignore-<outputs>
----------------

.. highlight:: dune

.. dune:action:: ignore-<outputs>
   :param: <DSL>

   Ignore the output, where ``<outputs>`` is one of: ``stdout``, ``stderr``, or
   ``outputs``.

   Example::

     (ignore-stderr
      (run ./get-conf.exe))

###################
 with-<outputs>-to
###################

.. highlight:: dune

.. dune:action:: with-<outputs>-to
   :param: <file> <DSL>

   Redirect the output to a file, where ``<outputs>`` is one of:
   ``stdout``, ``stderr`` or ``outputs`` (for both ``stdout`` and
   ``stderr``).

   Example:

   .. code::

      (with-stdout-to conf.txt
       (run ./get-conf.exe))

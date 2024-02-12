with-stdin-from
---------------

.. highlight:: dune

.. dune:action:: with-stdin-from
   :param: <file> <DSL>

   Redirect the input from a file.

   Example::

     (with-stdin-from data.txt
      (run ./tests.exe))

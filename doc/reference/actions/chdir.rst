chdir
-----

.. highlight:: dune

.. describe:: (chdir <dir> <DSL>)

   Run an action in a different directory.

   Example::

     (chdir src
      (run ./build.exe))

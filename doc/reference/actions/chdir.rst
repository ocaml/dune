#######
 chdir
#######

.. highlight:: dune

.. dune:action:: chdir
   :param: <dir> <DSL>

   Run an action in a different directory.

   Example:

   .. code::

      (chdir src
       (run ./build.exe))

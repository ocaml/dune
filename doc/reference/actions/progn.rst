#######
 progn
#######

.. highlight:: dune

.. dune:action:: progn
   :param: <DSL>...

   Execute several commands in sequence.

   Example:

   .. code::

      (progn
       (run ./proga.exe)
       (run ./progb.exe))

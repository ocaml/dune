#############
 dynamic-run
#############

.. highlight:: dune

.. dune:action:: dynamic-run
   :param: <prog> <args>

   Execute a program that was linked against the ``dune-action-plugin``
   library. ``<prog>`` is resolved in the same way as in :doc:`run`.

   Example:

   .. code::

      (dynamic-run ./plugin.exe)

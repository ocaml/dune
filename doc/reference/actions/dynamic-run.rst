dynamic-run
-----------

.. highlight:: dune

.. dune:action:: dynamic-run
   :param: <prog> <args>

   Execute a program that was linked against the ``dune-action-plugin`` library.
   ``<prog>`` is resolved in the same way as in :dune:ref:`action-run`.

   Example::

   (dynamic-run ./plugin.exe)

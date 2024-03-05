dynamic-run
-----------

.. highlight:: dune

.. describe:: (dynamic-run <prog> <args>)

   Execute a program that was linked against the ``dune-action-plugin`` library.
   ``<prog>`` is resolved in the same way as in :doc:`run`.

   Example::

   (dynamic-run ./plugin.exe)

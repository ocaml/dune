dynamic-run
-----------

.. highlight:: dune

.. describe:: (dynamic-run <prog> <args>)

   Execute a program using ``Dune_rpc.V1.Action_plugin``, such as the
   ``Dune_rpc_lwt.V1.Action_plugin`` implementation. ``<prog>`` is resolved in
   the same way as in :doc:`run`.

   The program remains running while Dune builds dependencies that it discovers,
   so each ``dynamic-run`` invocation starts the program only once.

   Example::

   (dynamic-run ./plugin.exe)

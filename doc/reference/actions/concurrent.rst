concurrent
----------

.. highlight:: dune

.. describe:: (concurrent <DSL> ...)

   Execute several commands concurrently and collect all resulting errors, if any.

   .. warning:: The concurrency is limited by the ``-j`` flag passed to Dune.
      In particular, if Dune is running with ``-j 1``, these commands will
      actually run sequentially, which may cause a deadlock if they talk to
      each other.

   Example::

     (concurrent
      (run ./proga.exe)
      (run ./progb.exe))

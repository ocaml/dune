with-accepted-exit-codes
------------------------

.. highlight:: dune

.. describe:: (with-accepted-exit-codes <pred> <DSL>)

   .. versionadded:: 2.0

   Specifies the list of expected exit codes for the programs executed in
   ``<DSL>``. ``<pred>`` is a predicate on integer values, and it's specified
   using the :doc:`/reference/predicate-language`. ``<DSL>`` can only contain
   nested occurrences of ``run``, ``bash``, ``system``, ``chdir``, ``setenv``,
   ``ignore-<outputs>``, ``with-stdin-from``, and ``with-<outputs>-to``.

   Example::

     (with-accepted-exit-codes
      (or 1 2)
      (run false))

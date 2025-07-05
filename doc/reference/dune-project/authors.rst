authors
-------

.. describe:: (authors <strings>)

   .. versionadded:: 1.9

   Specify authors.

   Valid for all packages defined in the current Dune project. May be overriden
   by the per-package field (see :doc:`package`).

   Example:

   .. code:: dune

      (authors
       "Jane Doe <jane.doe@example.com>"
       "John Doe <john.doe@example.com>")

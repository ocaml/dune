maintainers
-----------

.. describe:: (maintainers <strings>)

   .. versionadded:: 1.10

   Specify maintainers.

   Valid for all packages defined in the current Dune project. May be overriden
   by the per-package field (see :doc:`package`).

   Example:

   .. code:: dune

      (maintainers
       "Jane Doe <jane.doe@example.com>"
       "John Doe <john.doe@example.com>")

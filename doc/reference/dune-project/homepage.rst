homepage
--------

.. describe:: (homepage <url>)

   .. versionadded:: 1.10

   The homepage of the project.

   If a hosting service is used in ``(source)``, a default value is provided.

   Valid for all packages defined in the current Dune project. May be overriden
   by the per-package field (see :doc:`package`).

   Example:

   .. code:: dune

      (bug_reports https://example.com/)

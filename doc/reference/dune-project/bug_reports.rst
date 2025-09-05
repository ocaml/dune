bug_reports
-----------

.. describe:: (bug_reports <url>)

   .. versionadded:: 1.10

   Where bugs should be reported.

   If a hosting service is used in ``(source)``, a default value is provided.

   Valid for all packages defined in the current Dune project. May be overriden
   by the per-package field (see :doc:`package`).

   Example:

   .. code:: dune

      (bug_reports https://dev.example.com/project/issues)

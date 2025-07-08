documentation
-------------

.. describe:: (documentation <url>)

   .. versionadded:: 1.10

   Where the documentation is hosted.

   Valid for all packages in the current Dune project. May be overriden by the
   per-package field (see :doc:`package`).

   .. versionadded:: 3.20

   For consistency with the per-package field, the following syntax is also
   allowed: ``(documentation (url <url>))``. However, the ``depends`` field is
   only allowed in the per-package version of the documentation stanza.

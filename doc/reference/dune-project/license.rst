license
-------

.. describe:: (license <strings>)

   .. versionadded:: 1.9

   Specify the license of the project, ideally as an identifier from the `SPDX
   License List <https://spdx.org/licenses/>`__.

   Valid for all packages defined in the current Dune project. May be overriden
   by the per-package field (see :doc:`package`).

   Example:

   .. code:: dune

      (license MIT)

   Multiple licenses may be specified.

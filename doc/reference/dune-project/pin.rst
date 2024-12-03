pin
---

.. warning::

   Dune Package Management is not final yet and the configuration options
   are subject to change.


Pins are package overrides used in the context of package management. They
allow to fix a package at a specific version which is not affected by the
package repositories selected.

.. describe:: (pin ...)

   .. versionadded:: 3.14

   Define a package override.

   .. describe:: (url <string>)

      The URL of the package source.

      This must be specified.

   .. describe:: (package ...)

      Defines which package is to be pinned.

      This must be specified.

      .. describe:: (name <string>)

         The name of the package.

         This must be specified.

      .. describe:: (version <string>)

         The version that the package should be assumed to be.

.. seealso:: :doc:`</reference/dune-workspace/pin> stanza in dune-workspace` for
   workspace-wide pinning.

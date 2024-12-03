pin
---

.. warning::

   :doc:`Dune Package Management </explanation/package-management>` is not
   final yet and the configuration options are subject to change.

Pins are package overrides used in the context of package management. They
allow to fix a package at a specific version which is not affected by the
package repositories selected.

.. describe:: (pin ...)

   .. versionadded:: 3.14

   Define a package override.

   .. describe:: (url <string>)

      The URL of the package source.

      This can be a path to a directory on the local file system or remote Git
      repository. Local paths can be absolute or relative, and may optionally
      begin with ``file://`` though this is not necessary. Remote Git
      repository URLs must begin with ``git+``, for example
      ``git+https://github.com/user/repo`` or
      ``git+git@github.com:user/repo.git``.

      This must be specified.

   .. describe:: (package ...)

      Defines which package is to be pinned.

      This must be specified.

      .. describe:: (name <string>)

         The name of the package.

         This must be specified.

      .. describe:: (version <string>)

         The version that the package should be assumed to be. Defaults to
         ``dev`` if unspecified.

.. seealso:: :doc:`pin stanza in dune-workspace </reference/dune-workspace/pin>` for
   workspace-wide pinning.

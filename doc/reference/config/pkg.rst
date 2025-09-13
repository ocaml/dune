pkg
---

Controls whether Dune's package management features are enabled. Package
management includes dependency locking, package resolution, and building 
dependencies from source.

.. code:: dune

    (pkg <setting>)

where ``<setting>`` is one of:

- ``enabled`` forces package management to be enabled, even if no lock
  directories are present.

- ``disabled`` forces package management to be disabled, even if lock
  directories are present. When disabled, Dune will not load package rules
  and all ``dune pkg`` commands will fail with an error.

If no ``(pkg ...)`` setting is specified, Dune will auto-detect whether to
enable package management based on the presence of lock directories in the
workspace. This usage is not currently recommended, since lock directories 
are not stabilized or portable.

.. versionadded:: 3.20

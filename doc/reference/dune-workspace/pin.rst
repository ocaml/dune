pin
===

.. warning::

   :doc:`Dune Package Management </explanation/package-management>` is not
   final yet and the configuration options are subject to change.

This stanza is used to define additional package sources to use when locking a
project and used for building dependencies.

.. note::

   Defining a pin does not enable it by default. It needs to be enabled in a
   lock directory using the :doc:`/reference/dune-workspace/lock_dir` stanza.

.. describe:: (pin ...)

   .. versionadded:: 3.15

   Defines a new package source.

   .. describe:: (name <string>)

      The name of the newly defined pin. This can be anything, it does not
      have to match the package.

      This must be specified.

   .. describe:: (url <string>)

      This can be a path to a directory on the local file system or remote Git
      repository. Local paths can be absolute or relative, and may optionally
      begin with ``file://`` though this is not necessary. Remote Git
      repository URLs must begin with ``git+``, for example
      ``git+https://github.com/user/repo`` or
      ``git+git@github.com:user/repo.git``.

      This must be specified.

   .. describe:: (package ...)

      Specifies the packages to assign this pin to.

      .. describe:: (name <string>)

         The name of the package.

         This must be specified.

      .. describe:: (version <string>)

         The version that the package should be assumed to be. Defaults to
         ``dev`` if unspecified.

.. seealso:: :doc:`pin stanza in dune-project </reference/dune-project/pin>` for
   per-project pins.

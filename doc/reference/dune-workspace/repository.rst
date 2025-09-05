repository
==========

.. warning::

   :doc:`Dune Package Management </explanation/package-management>` is not
   final yet and the configuration options are subject to change.

This stanza defines a new named package repository and attaches a source
location to it.

.. note::

   Defining a repository does not enable it in project by default. It needs to be
   enabled in a lock directory using the :doc:`/reference/dune-workspace/lock_dir`
   stanza.

.. describe:: (repository ...)

   .. versionadded:: 3.12

   Defines a named package repository.

   .. describe:: (name <string>)

      The name used to refer to the repository. Names have to be unique.

      This must be specified.

   .. describe:: (url <string>)

      The location from which the repository will be loaded.

      Both HTTP and Git locations can be specified, the latter allowing for
      extensive control of the version by specifying an exact revision, tag or
      branch.

      This must be specified.

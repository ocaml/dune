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

      The URL scheme for Git locations need to specify the backend as git in
      the scheme part of the url, for instance:

          git+https://github.com/oxcaml/opam-repository.git

      You can also use the SSH based URLs as follows:

          git+ssh://git@github.com/oxcaml/opam-repository

      NOTE that the SSH URLs provided in the GitHub UI use a ``:`` to separate
      the repository owner and name from the hostname, but this needs to be
      replaced with a ``/`` to work correctly. ``github.com:<username>/<repo>``
      needs to be changed to ``github.com/<username>/<repo>``.

      If the Git URLs need authentication, the user needs to set it up locally
      so ``git`` can authenticate and access these repositories.

      This must be specified.

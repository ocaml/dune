source
------

.. describe:: (source ...)

   .. versionadded:: 1.7

   Specify where the source for the package can be found.

   It can be specified as ``(uri <uri>)`` or using shortcuts for some
   hosting services:

   Valid for all packages defined in the current Dune project. May be overriden
   by the per-package field (see :doc:`package`).

   .. list-table::

     * - Service
       - Syntax
     * - `GitHub <https://github.com>`_
       - ``(github user/repo)``
     * - `Bitbucket <https://bitbucket.org>`_
       - ``(bitbucket user/repo)``
     * - `GitLab <https://gitlab.com>`_
       - | ``(gitlab user/repo)``
         | ``(gitlab organization/project/repo)`` *(New in 3.17)*
     * - `SourceHut <https://sr.ht>`_
       - ``(sourcehut user/repo)``
     * - `Codeberg <https://codeberg.org>`_
       - ``(codeberg user/repo)`` *(New in 3.17)*
     * - `Tangled <https://tangled.org>`_
       - ``(tangled @user.domain/repo)`` *(New in 3.21)*

   Examples:

   .. code:: dune

      (source
       (github ocaml/dune))

   .. code:: dune

      (source
       (uri https://dev.example.com/project.git))

generate_opam_files
-------------------

.. describe:: (generate_opam_files ...)

   .. versionadded:: 1.10

   Use metadata specified in the ``dune-project`` file to generate ``.opam``
   files.

   To enable this integration, add the following field to the ``dune-project``
   file:

   .. code:: dune

      (generate_opam_files)

   .. seealso:: :doc:`/howto/opam-file-generation`

Dune uses the following global fields to set the metadata for all packages
defined in the project:

.. describe:: (license <strings>)

   .. versionadded:: 1.9

   Specify the license of the project, ideally as an identifier from the `SPDX
   License List <https://spdx.org/licenses/>`__.

   Example:

   .. code:: dune

      (license MIT)

   Multiple licenses may be specified.

.. describe:: (authors <strings>)

   .. versionadded:: 1.9

   Specify authors.

   Example:

   .. code:: dune

      (authors
       "Jane Doe <jane.doe@example.com>"
       "John Doe <john.doe@example.com>")

.. describe:: (maintainers <strings>)

   .. versionadded:: 1.10

   Specify maintainers.

   Example:

   .. code:: dune

      (maintainers
       "Jane Doe <jane.doe@example.com>"
       "John Doe <john.doe@example.com>")

.. describe:: (source ...)

   .. versionadded:: 1.7

   Specify where the source for the package can be found.

   It can be specified as ``(uri <uri>)`` or using shortcuts for some
   hosting services:

   .. list-table::

     * - Service
       - Syntax
     * - `Github <https://github.com>`_
       - ``(github user/repo)``
     * - `Bitbucket <https://bitbucket.org>`_
       - ``(bitbucket user/repo)``
     * - `Gitlab <https://gitlab.com>`_
       - | ``(gitlab user/repo)``
         | ``(gitlab organization/project/repo)`` *(New in 3.17)*
     * - `Sourcehut <https://sr.ht>`_
       - ``(sourcehut user/repo)``
     * - `Codeberg <https://codeberg.org>`_
       - ``(codeberg user/repo)`` *(New in 3.17)*

   Examples:

   .. code:: dune

      (source
       (github ocaml/dune))

   .. code:: dune

      (source
       (uri https://dev.example.com/project.git))

.. describe:: (bug_reports <url>)

   .. versionadded:: 1.10

   Where bugs should be reported.

   If a hosting service is used in ``(source)``, a default value is provided.

   Example:

   .. code:: dune

      (bug_reports https://dev.example.com/project/issues)

.. describe:: (homepage <url>)

   .. versionadded:: 1.10

   The homepage of the project.

   If a hosting service is used in ``(source)``, a default value is provided.

   Example:

   .. code:: dune

      (bug_reports https://example.com/)

.. describe:: (documentation <url>)

   .. versionadded:: 1.10

   Where the documentation is hosted.

With these fields, every time one calls Dune to execute some rules (either via
``dune build``, ``dune runtest``, or something else), the opam files get
generated.

Some or all of these fields may be overridden for each package of the project,
see :doc:`/reference/dune-project/package`.

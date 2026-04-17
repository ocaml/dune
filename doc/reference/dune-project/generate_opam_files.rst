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

With this field, Dune can generate ``*.opam`` files from ``dune-project``
metadata. Build the :doc:`/reference/aliases/opam` alias to refresh them
explicitly:

.. code:: console

   $ dune build @opam

The :doc:`/reference/aliases/install` and :doc:`/reference/aliases/runtest`
aliases also depend on ``@opam``.

With ``(lang dune 3.23)`` or newer, updates to checked-in ``*.opam`` files are
reported as promotions and must be applied with ``dune promote``.

With older Dune language versions, generated ``*.opam`` files are promoted to
the source tree automatically when these aliases are built.

To add opam fields not directly supported by ``dune-project``, you can use
``<package-name>.opam.template`` files (see :doc:`/reference/packages`).

.. seealso::

   :doc:`/reference/dune-project/package` for fields supported by
   ``dune-project``

   :doc:`/reference/packages` for more information on ``opam.template`` files

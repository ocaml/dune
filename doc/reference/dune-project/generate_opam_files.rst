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

With this field, every time one calls Dune to execute some rules (either via
``dune build``, ``dune runtest``, or something else), the opam files get
generated.

To add opam fields not directly supported by ``dune-project``, you can use
``<package-name>.opam.template`` files (see :doc:`/reference/packages`).

.. seealso::

   :doc:`/reference/dune-project/package` for fields supported by
   ``dune-project``

   :doc:`/reference/packages` for more information on ``opam.template`` files

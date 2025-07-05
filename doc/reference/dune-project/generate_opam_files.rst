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

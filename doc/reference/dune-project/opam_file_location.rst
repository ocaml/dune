opam_file_location
------------------

.. describe:: (opam_file_location <location>)

   .. versionadded:: 3.8

   Configure where generated ``.opam`` files are located. `<location>` can
   be one of the following:

   - ``relative_to_project``: the ``.opam`` files are generated in the project
     root directory. This is the default.

   - ``inside_opam_directory``: the ``.opam`` files are generated in a directory
     named ``opam`` in the project root directory.

   .. seealso:: :doc:`/howto/opam-file-generation`

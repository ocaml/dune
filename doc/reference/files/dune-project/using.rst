using
-----

.. describe:: (using <plugin> <version>)

   Enable a dune language extension.

   The language of configuration files read by Dune can be extended to support
   additional stanzas (e.g., ``menhir``, ``coq.theory``, ``mdx``).

   `<plugin>` is the name of the plugin that defines this stanza and
   `<version>` describes the configuration language's version. Note that this
   version has nothing to do with the version of the associated tool or
   library. In particular, adding a ``using`` stanza will not result in a build
   dependency in the generated ``.opam`` file. See :doc:`generate_opam_files`.

   Example:

   .. code:: dune

      (using mdx 0.3)

files
-----

.. versionadded:: 3.21

The ``files`` stanza allows restricting which files Dune should consider in the
current directory. Its syntax mirrors the :doc:`/reference/predicate-language`
used by the ``dirs`` stanza and supports ``:standard`` (which expands to all
files), globs, and set operations.

This is useful in mixed build setups where external tools such as ``make``
produce artifacts that Dune should ignore.

Examples:

.. code:: dune

   (files :standard \ *.cm*)   ;; ignore bytecode/native artifacts
   (files *.ml *.mli)          ;; only keep OCaml sources

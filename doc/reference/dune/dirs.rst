dirs
----

.. versionadded:: 1.6

The ``dirs`` stanza allows specifying the subdirectories Dune will include in a
build. The syntax is based on Dune's :doc:`/reference/predicate-language` and
allows the following operations:

- The special value ``:standard`` which refers to the default set of used
  directories. These are the directories that don't start with ``.`` or ``_``.

- Set operations. Differences are expressed with backslash: ``* \ bar``; unions
  are done by listing multiple items.

- Sets can be defined using globs.

Examples:

.. code:: dune

   (dirs *) ;; include all directories
   (dirs :standard \ ocaml) ;; include all dirs except ocaml
   (dirs :standard \ test* foo*) ;; exclude all dirs that start with test or foo

Dune will not scan a directory that isn't included in this stanza. Any contained
``dune`` (or other special) files won't be interpreted either. It is still
possible to depend on a specific file inside an excluded subdirectory by naming
that file explicitly, but Dune won't discover files there by traversing the
subdirectory.

Use :doc:`/reference/dune/data_only_dirs` when Dune should ignore ``dune`` files
inside a subdirectory but still treat the subdirectory's files as source files
for dependency discovery. A data-only directory must still be included by
``dirs``; if ``dirs`` excludes it, it is ignored rather than data-only.

.. warning::

  Directory names should not contain any trailing slashes.

If you want to exclude a subdirectory, such as `foo/bar`, you need to use the
:doc:`/reference/dune/subdir` stanza:

.. code:: dune

  (subdir foo (dirs :standard \ bar)) ;; exclude foo/bar

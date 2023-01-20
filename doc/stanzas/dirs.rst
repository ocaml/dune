.. _dune-subdirs:

dirs
----

.. versionadded:: 1.6

The ``dirs`` stanza allows specifying the subdirectories Dune will include in a
build. The syntax is based on Dune's :ref:`predicate-lang` and allows the user
the following operations:

- The special value ``:standard`` which refers to the default set of used
  directories. These are the directories that don't start with ``.`` or ``_``.

- Set operations. Differences are expressed with backslash: ``* \ bar``; unions
  are done by listing multiple items.

- Sets can be defined using globs.

Examples:

.. code:: lisp

   (dirs *) ;; include all directories
   (dirs :standard \ ocaml) ;; include all dirs except ocaml
   (dirs :standard \ test* foo*) ;; exclude all dirs that start with test or foo

Dune will not scan a directory that isn't included in this stanza. Any contained
``dune`` (or other special) files won't be interpreted either and will be
treated as raw data. It is however possible to depend on files inside ignored
subdirectories.

data_only_dirs
--------------

.. versionadded:: 1.6

Dune allows the user to treat directories as *data only*. ``dune`` files in
these directories won't be evaluated for their rules, but the contents of these
directories will still be usable as dependencies for other rules.

The syntax is a list of immediate subdirectory names or globs. Unlike the
:doc:`dirs` stanza, this field doesn't support the full predicate language:
``:standard``, ``\``, and other set operations aren't available here.

``data_only_dirs`` only changes the status of directories that Dune would
otherwise include. If a directory is excluded by ``dirs``, then Dune ignores it
rather than treating it as data-only. For example, this marks ``vendor`` as
ignored, not data-only, because it isn't included by ``dirs``:

.. code:: dune

   (dirs src)
   (data_only_dirs vendor)

To make ``vendor`` data-only while excluding another generated directory, keep
``vendor`` in ``dirs`` and mark it with ``data_only_dirs``:

.. code:: dune

   (dirs :standard \ target)
   (data_only_dirs vendor)

Example:

.. code:: dune

   ;; dune files in fixtures_* dirs are ignored
   (data_only_dirs fixtures_*)

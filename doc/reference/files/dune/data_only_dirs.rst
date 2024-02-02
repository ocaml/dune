data_only_dirs
--------------

.. versionadded:: 1.6

Dune allows the user to treat directories as *data only*. ``dune`` files in
these directories won't be evaluated for their rules, but the contents of these
directories will still be usable as dependencies for other rules.

The syntax is the same as for the ``dirs`` stanza except that ``:standard`` is
empty by default.

Example:

.. code:: dune

   ;; dune files in fixtures_* dirs are ignored
   (data_only_dirs fixtures_*)

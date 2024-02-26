vendored_dirs
-------------

.. versionadded:: 1.11

Dune supports vendoring other Dune-based projects natively, since simply copying
a project into a subdirectory of your own project will work. Simply doing that
has a few limitations though. You can workaround those by explicitly marking
such directories as containing vendored code.

Example:

.. code:: dune

   (vendored_dirs vendor)


Dune will not resolve aliases in vendored directories. By default, it won't
build all installable targets, run the tests, format, or lint the code located
in such a directory while still building your project's dependencies. Libraries
and executables in vendored directories will also be built with a ``-w -a`` flag
to suppress all warnings and prevent pollution of your build output.

ignored_subdirs
---------------

.. deprecated:: 1.6

One may also specify *data only* directories using the ``ignored_subdirs``
stanza, meaning it's the same as ``data_only_dirs``, but the syntax isn't as
flexible and only accepts a list of directory names. It's advised to switch to
the new ``data_only_dirs`` stanza.

Example:

.. code:: dune

     (ignored_subdirs (<sub-dir1> <sub-dir2> ...))

All of the specified ``<sub-dirn>`` will be ignored by Dune. Note that users
should rely on the ``dirs`` stanza along with the appropriate set operations
instead of this stanza. For example:

.. code:: dune

  (dirs :standard \ <sub-dir1> <sub-dir2> ...)

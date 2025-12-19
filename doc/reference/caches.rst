Dune Caches
===========

Dune implements several different caches:

- The build cache contains build artifacts that are shared across different
  workspaces. This is the main cache, interacted with via the ``dune cache``
  command.
- The toolchains cache contains the compiler (and compiler variants) when they
  can't be installed like regular packages, due to them not being relocatable.
- The revision store is a git repository containing all revisions fetched via
  git.

Their location is as follows:

+------------------+----------------+------------------------------------+-----------------------------------+
| Name             | Default status | Default location on Unix           | Alternate location (has priority) |
+==================+================+====================================+===================================+
| Build cache      | Enabled        | ``XDG_CACHE_HOME/dune/db``         | ``DUNE_CACHE_ROOT/db``            |
+------------------+----------------+------------------------------------+-----------------------------------+
| Toolchains cache | Enabled        | ``XDG_CACHE_HOME/dune/toolchains`` | ``DUNE_CACHE_ROOT/toolchains``    |
+------------------+----------------+------------------------------------+-----------------------------------+
| Revision store   | Enabled        | ``XDG_CACHE_HOME/dune/git-repo``   | ``DUNE_CACHE_ROOT/git-repo``      |
+------------------+----------------+------------------------------------+-----------------------------------+

.. note::

   On Windows, you can replace ``XDG_CACHE_HOME`` by ``%LOCALAPPDATA%\Microsoft\Windows\Temporary Internet Files``


.. TODO(diataxis) Above is reference material, below is explanation.

How Caching Works
=================

Before executing a build rule, Dune hashes the rule and looks up that hash in
the shared cache, and if it finds a matching entry, Dune skips the rule's
execution and restores the results in the current build directory. This can
greatly speed up builds when different workspaces share code, as well as when
switching branches or simply undoing some changes within the same workspace.


Configuration
=============

There are three ways to configure the Dune cache.  Choose the one that is more
convenient for you:

* Add ``(cache <setting>)`` to your Dune configuration file
  (``~/.config/dune/config`` by default).
* Set the environment variable ``DUNE_CACHE`` to ``<setting>``
* Run Dune with the ``--cache=<setting>`` flag.

Here, ``<setting>`` must be one of:

* ``disabled``: disables the Dune cache completely.

* ``enabled-except-user-rules``: enables the Dune cache, but excludes
  user-written rules. This setting is a conservative choice that can avoid
  breaking rules whose dependencies are not correctly specified. Currently the
  default.

* ``enabled``: enables the Dune cache unconditionally.

By default, Dune stores the cache in your ``XDG_CACHE_HOME`` directory on \*nix
systems and ``%LOCALAPPDATA%\Microsoft\Windows\Temporary Internet Files\dune`` on Windows.
You can change the default location by setting the environment variable
``DUNE_CACHE_ROOT``.


Cache Storage Mode
==================

Dune supports two modes of storing and restoring cache entries: `hardlink` and
`copy`. If your file system supports hard links, we recommend that you use the
`hardlink` mode, which is generally more efficient and reliable.

The `hardlink` Mode
-------------------

By default, Dune uses hard links when storing and restoring cache entries. This
is fast and has zero disk space overhead for files that still live in a build
directory. There are two disadvantages of this mode:

* The cache storage must be on the same partition as the build tree.
* A cache entry can be corrupted by modifying the hard link that points to it
  from the build directory. To reduce the risk of cache corruption, Dune
  systematically removes write permissions from all build results. It is worth
  noting that modifying files in the build directory is a bad practice anyway.

The `copy` Mode
---------------

If you specify ``(cache-storage-mode copy)`` in the configuration file, Dune
will copy files to and from the cache instead of using hard links. This mode is
slower and has higher disk space usage. On the positive side, it is more
portable and doesn't have the disadvantages of the `hardlink` mode (see above).

You can also set or override the storage mode via the environment variable
``DUNE_CACHE_STORAGE_MODE`` and the command line flag ``--cache-storage-mode``.

Trimming the Cache
==================

Storing all historically produced build results in the cache is infeasible, so
you'll need to occasionally trim the cache. To do that, run the ``dune cache
trim --size=BYTES`` command. This will remove the oldest used cache entries to
keep the cache overhead below the specified size. By "overhead" we mean the
cache entries whose hard link count is equal to 1, i.e., which aren't used in
any build directory. Trimming cache entries whose hard link count is greater
than 1 would not free any disk space.

Note that previous versions of Dune, cache provided a "cache daemon" that could
periodically trim the cache. The current version doesn't require an additional
daemon process, so this automated trimming functionality is no longer provided.


Reproducibility
===============

Reproducibility Check
---------------------

While the main purpose of Dune cache is to speed up build times, it can also be
used to check build reproducibility. By specifying ``(cache-check-probability
FLOAT)`` in the configuration file, or running Dune with the
``--cache-check-probability=FLOAT`` flag, you instruct Dune to re-execute
randomly chosen build rules and compare their results with those stored in the
cache. If the results differ, the rule is not reproducible, and Dune will print
out a corresponding warning.

Non-Reproducible Rules
----------------------

Some build rules are inherently not reproducible because they involve running
non-deterministic commands that, for example, depend on the current time or
download files from the Internet. To prevent Dune from caching such rules, mark
them as non-reproducible by using ``(deps (universe))``. Please see
:doc:`/concepts/dependency-spec` for more details.

############
 Dune Cache
############

..
   TODO(diataxis) This is reference material with some explanation.

Dune implements a cache of build results that is shared across different
workspaces. Before executing a build rule, Dune looks it up in the
shared cache, and if it finds a matching entry, Dune skips the rule's
execution and restores the results in the current build directory. This
can greatly speed up builds when different workspaces share code, as
well as when switching branches or simply undoing some changes within
the same workspace.

***************
 Configuration
***************

For now, Dune cache is an opt-in feature. There are three ways to enable
it. Choose the one that is more convenient for you:

-  Add ``(cache enabled)`` to your Dune configuration file
   (``~/.config/dune/config`` by default).
-  Set the environment variable ``DUNE_CACHE`` to ``enabled``
-  Run Dune with the ``--cache=enabled`` flag.

By default, Dune stores the cache in your ``XDG_CACHE_HOME`` directory
on \*nix systems and ``%LOCALAPPDATA%\Microsoft\Windows\Temporary
Internet Files\dune`` on Windows. You can change the default location by
setting the environment variable ``DUNE_CACHE_ROOT``.

********************
 Cache Storage Mode
********************

Dune supports two modes of storing and restoring cache entries:
`hardlink` and `copy`. If your file system supports hard links, we
recommend that you use the `hardlink` mode, which is generally more
efficient and reliable.

The `hardlink` Mode
===================

By default, Dune uses hard links when storing and restoring cache
entries. This is fast and has zero disk space overhead for files that
still live in a build directory. There are two disadvantages of this
mode:

-  The cache storage must be on the same partition as the build tree.

-  A cache entry can be corrupted by modifying the hard link that points
   to it from the build directory. To reduce the risk of cache
   corruption, Dune systematically removes write permissions from all
   build results. It is worth noting that modifying files in the build
   directory is a bad practice anyway.

The `copy` Mode
===============

If you specify ``(cache-storage-mode copy)`` in the configuration file,
Dune will copy files to and from the cache instead of using hard links.
This mode is slower and has higher disk space usage. On the positive
side, it is more portable and doesn't have the disadvantages of the
`hardlink` mode (see above).

You can also set or override the storage mode via the environment
variable ``DUNE_CACHE_STORAGE_MODE`` and the command line flag
``--cache-storage-mode``.

********************
 Trimming the Cache
********************

Storing all historically produced build results in the cache is
infeasible, so you'll need to occasionally trim the cache. To do that,
run the ``dune cache trim --size=BYTES`` command. This will remove the
oldest used cache entries to keep the cache overhead below the specified
size. By "overhead" we mean the cache entries whose hard link count is
equal to 1, i.e., which aren't used in any build directory. Trimming
cache entries whose hard link count is greater than 1 would not free any
disk space.

Note that previous versions of Dune, cache provided a "cache daemon"
that could periodically trim the cache. The current version doesn't
require an additional daemon process, so this automated trimming
functionality is no longer provided.

*****************
 Reproducibility
*****************

Reproducibility Check
=====================

While the main purpose of Dune cache is to speed up build times, it can
also be used to check build reproducibility. By specifying
``(cache-check-probability FLOAT)`` in the configuration file, or
running Dune with the ``--cache-check-probability=FLOAT`` flag, you
instruct Dune to re-execute randomly chosen build rules and compare
their results with those stored in the cache. If the results differ, the
rule is not reproducible, and Dune will print out a corresponding
warning.

Non-Reproducible Rules
======================

Some build rules are inherently not reproducible because they involve
running non-deterministic commands that, for example, depend on the
current time or download files from the Internet. To prevent Dune from
caching such rules, mark them as non-reproducible by using ``(deps
(universe))``. Please see :doc:`concepts/dependency-spec`.

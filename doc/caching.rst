*******
Caching
*******

Dune has the ability to cache built files for later retrieval. This
can greatly speedup subsequent builds when some dependencies are
rebuilt in different workspaces, switching branches or iterating on
code back and forth.


Configuration
=============

The cache is, for now, an opt-in feature. Add `(cache enabled)` to
your dune configuration file (default `~/.config/dune/config`) to
activate it. When turned on, built files will automatically be
promoted to the cache, and subsequent builds will automatically check
the cache for hits.

The cached files are stored inside you `XDG_CACHE_HOME` directory on
\*nix systems, and `"HOME\\Local Settings\\Cache"` on Windows.


Daemon
======

By default, most cache operations go through the dune cache daemon, a
separate process that dune instances connect to. This enables
promotions to happen asynchronously and not slow the build
process. The daemon is automatically started if needed when dune needs
accessing the cache, and lives on for further use.

Although the daemon concept is totally transparent, one can control it
via the `dune cache` subcommand.

Starting the daemon
-------------------

Use `dune cache start` to start the caching daemon if not running and
print its endpoint, or retrieve the endpoint of the currently running
daemon otherwise. A notable option is `--foreground` to not detach the
daemon, which can help inspecting its log output.

Stopping the daemon
-------------------

Use `dune cache stop` to stop the caching daemon. Although the daemon,
when idle, should consume zero resources, you may want to get rid of
the process. Also useful to restart the daemon with `--foreground`.


Filesystem implementation
=======================================

Hardlink mode
-------------

By default the cache works by creating hardlinks to built files inside
the cache directory when promoted, and in other build trees when
retrieved. This has the great advantage of having zero disk space
overhead for files still living in a build directory. This has two
main constraints:

* The cache root must be on the same partition as the build tree.
* Produced files will be stripped from write permissions, as they are
  shared between build trees. Note that modifying built files is bad
  practice in any case.

Copy mode
---------

If one specifies `(cache-duplication copy)` in the configuration file,
dune will copy files to and from the cache instead of using hardlinks.
This can be useful if the build cache is on a different partition.

On-disk size
============

The cache daemon will perform periodic trimming to limit the overhead.
Every 10 minutes, it will purge the least recently used files so the
cache overhead does not exceed 10G. This is configurable through the
`(cache-trim-period SECONDS)` and `(cache-trim-size BYTES)`
configuration entries. Note that this operation will only consider the
cache overhead, i.e. files not currently hard-linked in a build
directory, as removing files currently used would not free any disk
space.

On can run `dune cache trim --size=BYTES` to manually trigger trimming
in the cache daemon.


Reproducibility check
=====================

While default mode of operation of the cache is to speedup build times
by not re-running some rules, it can also be used to check build
reproducibility. If `(cache-check-probability FLOAT)` or
`--cache-check-probability=FLOAT` is specified either respectively in
the configuration file or the command line, in case of a cache hit
dune will rerun the rule anyway with the given probability and compare
the resulting files against a potential cache hit. If the files
differ, the rule is not reproducible and a warning will be emitted.


Daemon-less mode
================

While the cache daemon provides asynchronous promotions to speedup
builds and background trimming amongst other things, in some
situations direct access can be preferable. This can be the case when
running in an isolated environment like Docker or OPAM sandboxes,
where only one instance of dune will ever be running at a time, and
access to external cache is prohibited. Direct filesystem access can
be obtained by specifying `(cache-transport direct)` in the
configuration file or passing `--cache-transport=direct` on the
command line.

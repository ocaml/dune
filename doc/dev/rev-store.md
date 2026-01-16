The Revision Store
==================

The revision store is the place where Git data that is relevant to the Dune
package management is cached.

The Concepts
------------

The revision store uses Git in the way of its original slogan, as a
content-addressable file system. A lot of data (source code, package metadata,
and other files) is stored in Git repositories that are often forked from each
other, hence to save space Dune has a Git object cache.

Git is designed to store revisions and address them by content hash. However,
these revisions do not have to have a common ancestor and given Git uses SHA1
hashes for addressing it is possible to join multiple repositories in one
single Git repository without clashing. A fairly common use case outside of
Dune is the `gh-pages` branch pattern used to serve documentation on GitHub,
where the documentation branch does not share a common parent with the main
branch of the repository.

The revision store exploits this feature by putting all revisions of all
repositories into one single large repository to take advantage of caching
effects.

The Advantages
--------------

This way of organizing means that all revisions shared between multiple forks
of a Git repository can reuse the same Git objects that are in common and don't
need to download them nor store them again. Updating repositories can be done
incrementally, as Git knows which revisions are available locally and which
ones need to be fetched.

It is also possible to refer to previous states easily as the commits are part
of a Git repository and can be accessed by their commit hash at any time.

Considerations and Compromises
------------------------------

An important consideration was that the management of the revision store should
be entirely transparent to the user, they should not need to do any steps to
create nor maintain it. It should get created automatically if needed and all
the steps that are necessary to keep it updated should happen in the
background. The store should always work like a cache that can be discarded
safely without causing data loss.

The revision store should always give out the most recent version of data,
unless explicitly instructed otherwise. This means that:

  * If only a Git source is specified, then the revision store will
    automatically get the newest revision
  * If the source specifies a tag or branch, then the revision store will
    automatically update to the newest revision
  * If a revision is specified, the revision store will only update if the
    revision is not yet cached, otherwise the cached version can be used

The final consideration means that offline usage is possible if all specified
repositories include their commit hash.

Due to the fact that the revision store is a Git repository it means that the
data sources that can be added to it also have to be available via Git. This
means that adding repositories that use different version control systems
aren't supported at the moment nor are plain HTTP sources supported.

Support for other kinds of VCSes is a possible extension by replicating similar
concepts with other version control systems, provided they allow for similar
flexibility as the Git way of storing revisions. However at the moment most
users have settled on using Git, hence this version should be able to
accommodate the needs for most users.

Another compromise is that fetching a commit requires downloading its
associated tree and blob objects, which may be larger than a pre-packaged
tarball for the same content. However, subsequent fetches benefit from
object deduplication since unchanged files share the same blob hashes.

Implementation
--------------

This section describes the current way the revision store is implemented.
The implementation is subject to change and this document may therefore lag
behind. The revision store is a singleton: only one instance exists per Dune
process, created lazily on first access.

### Requirements

The revision store requires Git version 2.29 or later. This is needed for the
`--no-write-fetch-head` flag which prevents unnecessary ref updates during
fetches.

When invoking Git, Dune sets `LC_ALL=C` to prevent localized output that would
be harder to parse, and `GIT_TERMINAL_PROMPT=0` to disable interactive prompts.
This means that repositories requiring interactive authentication (e.g., SSH
keys with passphrases, or HTTPS credentials) will fail unless credentials are
configured via Git credential helpers, SSH agent, or other non-interactive
means.

### Directory Structure

As the revision store is not project specific, it is stored in the user's cache
directory (using the [freedesktop.org](https://www.freedesktop.org/wiki/)
specifications, the directory specified by `XDG_CACHE_HOME`), with all dune
instances sharing one single revision store.

The revision store uses two directories within the cache:

  * `git-repo/`: The bare Git repository containing all fetched objects
  * `rev_store/`: An optional LMDB database for caching file contents and
    directory listings (see Caching below)

### Git Repository

The revision store itself is a `bare` Git repository (created with `git init
--bare`) without a worktree. Since objects from many different source
repositories are stored together, there is no single "main" revision to check
out. A worktree would waste disk space since Git tooling can construct any
revision's files on demand from the bare repository.

Unlike a traditional Git workflow, the revision store does not create named
remotes for each source repository. Instead, it fetches objects directly by
their SHA hash:

```
git fetch <url> <object-hash>
```

When a new source is encountered, Dune uses `git ls-remote <url>` to discover
available refs and their corresponding commit hashes. Both the ref mappings and
object existence checks are cached in memory for the duration of the Dune
process. When a specific revision is needed, Dune fetches just that object
directly from the URL without creating a persistent remote configuration.

This approach has several advantages:
  * No remote naming conflicts between different sources
  * Minimal metadata stored in the Git repository
  * Only the needed objects are fetched

### Revision Resolution

When a package specifies a Git source with a branch or tag name (rather than a
commit hash), Dune must resolve it to a concrete commit. The resolution
process is as follows:

  1. Query the remote with `git ls-remote <url>` to get all refs and their
     commit hashes. The results are cached in memory per URL.
  2. Look up the revision string in the refs:
     - First, try to find it as-is (e.g., `refs/heads/main`)
     - If not found, try both `refs/heads/<revision>` (branch) and
       `refs/tags/<revision>` (tag)
     - If both a branch and tag with that name exist but point to different
       commits, raise an ambiguity error with hints to use the full ref path
  3. If not found in remote refs, fall back to `git rev-parse` to check if
     it's a commit hash already present locally.
  4. Once resolved, fetch the commit object from the remote URL if not already
     present locally. If the object already exists, no network I/O is performed.

The `HEAD` ref is also extracted from `ls-remote` output to determine the
remote's default branch.

### Concurrency and Locking

Multiple Dune processes may access the revision store concurrently. To prevent
corruption during initialization, a file lock (`rev-store.lock`) is acquired.
The lock acquisition retries up to 50 times with 100ms delays (~5 seconds
total) before failing. The lock file contains the PID of the holding process
to aid debugging when lock contention occurs.

Within a single Dune process, individual object fetches are coordinated using
per-object fiber mutexes. This prevents redundant concurrent fetches of the
same object when multiple fibers request it simultaneously.

### Submodule Support

The revision store fully supports Git submodules. When accessing a repository
that contains submodules:

  1. The `.gitmodules` file is parsed using `git config --list --blob` to
     extract submodule definitions. Each submodule must specify both `path`
     and `url`.
  2. Each submodule's commit object is fetched from its configured source URL
  3. Files from submodules are represented as redirects pointing to the actual
     content in the submodule's tree

Errors are raised if:
  * A submodule definition is missing `path` or `url`
  * A submodule path doesn't exist in the repository tree
  * Multiple submodules reference the same path with different commits

Note that submodules are fetched sequentially rather than in parallel due to
lock management requirements. This allows Dune to present a unified view of
the repository including all submodule contents.

### Repository Views (At_rev)

Once a revision is fetched, Dune creates an `Rev_store.At_rev.t` value
representing an immutable view of the repository at that commit. This provides
a virtual filesystem interface that the package management system uses to read
opam files, list package contents, and access source files without extracting
the entire repository to disk.

The view pre-computes the full file listing using `git ls-tree -z --long -r`,
which provides file sizes along with paths and hashes. These sizes are used
when reading multiple files to split the concatenated `git show` output. The
view also builds an index mapping directories to their contents for efficient
lookups.

The `At_rev` interface provides:
  * `content`: Read a single file's content
  * `directory_entries`: List files in a directory (immediate or recursive).
    Symbolic links are returned as regular files.
  * `check_out`: Materialize the entire tree to disk for building. This creates
    tar archives via `git archive` (in parallel for submodules), extracts them
    to a temporary directory, then atomically renames to the target path.

This abstraction is used throughout the package system via the `Mount` module,
which provides a unified interface over both local paths and Git repositories.

When reading multiple file contents via `git show`, commands are batched to
respect platform-specific command line length limits (8KB on Windows, ~2MB on
Unix).

### Caching

The revision store includes an optional LMDB-based cache to speed up the
package solver, which repeatedly lists and inspects opam repositories in the
revision store. Without the cache, each solver run must call git to list
packages and read opam files. The cache persists these results across Dune
invocations:

  * `ls-tree` results: The file listing for each commit
  * File contents: The actual content of blob objects, keyed by their hash

When reading multiple files, the `content_of_files` function first checks the
cache for each file's hash, then fetches any uncached files in a single
batched `git show` call, and stores the results back in the cache.

The cache is currently disabled by default as it lacks cache versioning, which
means incompatible cache formats could cause issues after Dune upgrades. It can
be enabled by setting the environment variable
`DUNE_CONFIG__REV_STORE_CACHE=enabled`. When enabled, the cache uses up to 5GB
of disk space.


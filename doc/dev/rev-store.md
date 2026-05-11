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
  * Only the objects that are required are fetched

### Fetch Negotiation Refs

When Git fetches objects, it uses a negotiation protocol to determine what the
server needs to send. The client tells the server which objects it already has
(via "have" lines), and the server responds with only the missing objects. For
this negotiation to work efficiently, Git needs refs pointing to known commits.

Since the revision store is a bare repository without traditional remotes, we
create refs for each successfully fetched object, namespaced by an escaped form
of the source URL:

```
refs/dune-pkg/<escaped-url>/<object-hash>
```

The escaped URL replaces characters that are invalid in git refs or Windows
filenames (everything except alphanumeric and `-`) with underscores. For
example, `https://github.com/ocaml/dune.git` becomes
`https___github_com_ocaml_dune_git`. This namespacing ensures that when fetching
from a remote, only refs from that same remote are used for negotiation. Without
this, Git would send "have" lines for all objects in the revision store,
including those from unrelated repositories, which wastes bandwidth during the
negotiation phase.

During fetch, we use `--negotiation-tip=refs/dune-pkg/<escaped-url>/*` to tell
Git to only consider refs under the namespace for the URL being fetched. This
restricts negotiation to relevant commits only.

These refs accumulate over time but are lightweight (just pointers to existing
objects). They can be safely pruned if disk space becomes a concern, though
this would cause the next fetch to download more data than strictly necessary.

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

### Storage format

The cache stores raw bytes only. The `ls-tree` map's value is the literal
`git ls-tree -z --long -r` output (lines rejoined on the NUL separator
that git uses), and the `objects` map's value is the raw blob content. No
OCaml-side serialisation is involved. On a cache hit, `Entry.parse` runs
over the cached bytes to reconstruct `File.t` and `Commit.t` values, the
same parse that runs on a cache miss after the subprocess returns.

This is a deliberate choice. Earlier iterations of the cache stored the
parsed `File.Set.t * Commit.Set.t` via `Marshal.to_string ~sharing:true`.
That had two failure modes:

  * **OCaml compiler coupling.** `Marshal`'s binary format depends on the
    OCaml runtime's in-memory representation, especially with sharing.
    Two builds of the same dune source against different OCaml versions
    could produce mutually unreadable caches.
  * **Silent schema drift.** Any change to a type reachable from the
    marshalled value (`File.t`, `Commit.t`, the set comparator, even the
    representation of `Path.Local.t`) silently changed the on-disk format.
    Structurally compatible changes did not raise; they decoded wrong
    values.

Storing the raw subprocess output sidesteps both. The trade-off is that
parsing runs on every cache hit, but the parse is a single regex pass per
line and the cost is dwarfed by the millisecond-scale subprocess cost the
cache exists to avoid.

### Versioning

The cache directory is keyed by a manual version string:

  `$DUNE_CACHE_ROOT/rev_store/<version>/`

The current `version` is `v1`. Bump the constant in
`src/dune_pkg/rev_store.ml` whenever the bytes we write to the cache
change shape. That happens when the `git ls-tree` invocation changes
(different flags or a different command), when a map is added or
removed from the cache, or when the key or value `Lmdb.Conv` of an
existing map changes. Parser-only changes to `Entry.parse` are not
triggers, because the cached bytes are still the same git output; the
new parser just interprets them differently. OCaml-side type or field
renames are also not triggers, because nothing is marshalled.

Older version directories left behind by previous builds are dormant;
the running dune only ever reads and writes the current version, and
the user can remove stale directories manually.

### Future extensibility

The single-cache raw-bytes design is intentionally minimal but does not
constrain future use. There are vague plans to use LMDB for other
parsing caches (e.g. parsed dune or opam files). Those will need a real
encoder and decoder, since the cached value is no longer a natural
subprocess output. When that work happens, two paths are open:

  * **Add a new, independent cache.** A parsed-dune-files cache can live
    at a different path (e.g. `$DUNE_CACHE_ROOT/dune_files/<version>/`)
    with its own `version` constant and its own LMDB env. The rev-store
    cache is untouched.
  * **Migrate the rev-store cache itself.** Bump `version` from `v1` to
    `v2`, swap the `Lmdb.Conv.string` for a typed converter, and rewrite
    `Entry.parse` callers accordingly. Old data at `v1/` is abandoned
    without migration code; the path-keyed versioning makes that
    costless.

Either way, the encoder/decoder choice (something like a `Repr`-driven
binary format, canonical S-expressions, or hand-rolled per type) is a
local decision for whatever cache introduces it, not a project-wide
contract. The only thing that needs to stay constant for all caches is
that **`Marshal` is not used**, so the on-disk format remains independent
of the OCaml compiler.

The cache is enabled by default on 64-bit non-Windows platforms. It
defaults to disabled on 32-bit (the 5GB LMDB map size does not fit in
virtual address space) and on Windows (until we can determine that LMDB
is not detrimental to performance there). Users can override the default
either way via `DUNE_CONFIG__REV_STORE_CACHE=enabled` or
`DUNE_CONFIG__REV_STORE_CACHE=disabled`. When enabled, the cache uses up
to 5GB of disk space.


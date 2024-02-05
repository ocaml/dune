The Revision Store
==================

The revision store is the place where Git data that is relevant to the Dune
package management is cached.

The Concepts
------------

The revision store uses Git in the way of its original slogan, as a
content-addressable file system. A lot of data (code) and meta-data (opam files)
is stored in Git repositories that are often forked from each other, hence to
save space Dune has a Git object cache.

Git is implemented as a way to store revisions and being able to address them.
However, these revisions do not have to have a common ancestor and given Git
uses SHA1 hashes for addressing it is possible to join multiple repositories in
one single Git repository without clashing. A fairly common usecase outside of
Dune are `gh-pages` branches to serve documentation on Github, which do not
share a common parent with the main branch of the repository.

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
of a Git repo and checking out an older version is as simple as checking out
the current version of the files.

Considerations and Compromises
------------------------------

An important consideration was that the management of the revision store should
be entirely transparent to the user, they should not need to do any steps to
create nor maintain it. It should get created automatically if needed and all
the steps that are necessary to keep it updated should happen in the
background. The store should always work like a cache that can be discarded
safely without causing data loss.

The revision store should always give out the most recent version of data,
unless explicitly instructed otherwise. This means that :

  * If only a Git source is specified, then the revision store will
    automatically get the newest revision
  * If the source specifies a tag or branch, then the revision store will
    automatically update to the newest revision
  * It a revision is specified, the revision store will only update if the
    revision is not yet cached, otherwise the cached version can be used

The final consideration means that an offline usage is possible if all
repositories specified are specified with their hash.

Due to the fact that the revision store is a Git repository it means that the
data sources that can be added to it also have to be available via Git. This
means that adding repositories that use different version control systems
aren't supported at the moment nor are plain HTTP sources supported.

Support for other kinds of VCSes is a possible extension by replicating similar
concepts with other version control systems, provided they allow for similar
flexibility as the Git way of storing revisions. However at the moment most
users have settled on using Git, hence this version should be able to
accommodate the needs for most users.

Another compromise is that old repositories with long histories and large sizes
have to be cloned before use, thus increasing the size of the initial download
compared to the same metadata downloaded as a compressed tarball. Despite Git
compressing objects, the history of the repositories to be added does increase
the overhead.

A solution to this could be shallow clones which only contain the latest
revisions, however these have [shown to be
problematic](https://blog.cocoapods.org/Master-Spec-Repo-Rate-Limiting-Post-Mortem/)
thus for time being we are fetching the complete histories.

Implementation
--------------

This section describes the current way the revision store is implemented.

As the revision store is not project specific, it is stored in the user's cache
directory (using the [freedesktop.org](https://www.freedesktop.org/wiki/)
specifications, the directory specified by `XDG_CACHE_HOME`), with all dune
instances sharing one single revision store.

The revision store itself is a `bare` Git repository without a worktree. This
is because all repositories in the revision store are equal and checking out
one particular revision would be a waste of disk space as the Git tooling can
be used to construct any revisions out of the bare repository anyway.

Thus every source that is added to the revision store as a remote that tracks
the default branch (or, if a branch is specified explicitly, then that
branch) and fetched, thus storing the required revisions in the revision store.

The implementation of these features is a mix of calling the `git` binary and
implementing parts in OCaml. This means that the `git` binary is required on
the system. Possible future improvements could be using
[ocaml-git](https://github.com/mirage/ocaml-git) to avoid the dependency.

# Dune cache: design and implementation notes

This document describes main ideas behind the Dune cache as well as a few
subtleties of the implementation. This is a working document and it will be
updated as part of on-going development. Some of the described features are
currently still in development or unused, in particular, here we describe
support for two types of cache entries &ndash; artifacts and values &ndash; but
Dune currently doesn't store any value entries in the cache.

The design and implementation are based on a non-trivial assumption that there
are no hash collisions, for instance, that we will never come across two files
with different contents but with the same content hash. While this assumption is
common in the world of build systems and package managers, and is highly
unlikely to be violated by chance, one can manufacture hash collisions on
purpose, especially when using a weak hash like MD5.

## What we store in the cache

The cache stores build _artifacts_ and _values_.

* An _artifact_ is a file produced by a build rule. As any file, it has a name
  as well as content. Note that we treat a file's executable permission bit as
  part of its content.

* A _value_ is anything else produced during a build that is not written to a
  file but which is worth storing persistently between successive builds. A
  common example is a string written to the standard output by a build action.
  Such output-producing actions may run as part of a build rule or at an earlier
  stage when generating rules. Unlike artifacts, values have no names, yet they
  still have content.

## How we use the cache

The build system uses the cache to _store_ and _restore_ build artifacts and
values. Here is a typical interaction sequence for the case of artifacts:

* The build system is executing a build rule and has already identified all of
  its dependencies, thereby obtaining the _rule hash_. It uses it to make a
  _restore request_ to the cache.

* Now there are two cases:

    - The cache successfully restores all the artifacts of the rule, placing
      them into the build directory and returning their content hashes. The
      build system can skip building the artifacts and can use the obtained
      content hashes for deciding whether any dependent build rules need to be
      rerun. **This successful scenario is the only reason we use the cache.**

    - The cache fails to restore the artifacts, either because of an error or
      because it doesn't have an entry for the given rule hash. In this case,
      the build system needs to build the artifacts itself. On completion, it
      makes a _store request_ to the cache, providing a list of build artifacts
      (_file names_ in the build directory) as well as their _content hashes_.
      The cache stores the artifacts and after that the build system is allowed
      to continue with the build (but not before, since the cache requires
      exclusive access to the artifacts in the build directory). There can be a
      rare situation where the store request is declined because the given entry
      is already in the cache. How could this happen? We did try to restore it
      first! The reason is that the cache can be populated concurrently by
      multiple build systems and by the distributed cache daemon, so there can
      be a race between multiple systems adding the same entry to the cache. In
      this case, the cache will perform _deduplication_ of build artifacts by
      replacing them with hard links to the copies already stored in the cache
      (this is an example where the exclusive access is needed).

Build values are handled similarly; the main difference is that to identify a
cache entry we use the corresponding _action hash_ rather than the _rule hash_.
The only difference between rule hashes and action hashes is that the former
include the names of the produced artifacts into the hash, while the latter
do not, since values have no names.

## Cache storage format

Let `root` stand for the cache root directory. It has three main subdirectories.

* `root/meta/v3` stores _metadata files_, one per each historically executed
  build rule or value-producing action. (While this is a convenient mental
  model, in reality we need to occasionally remove some outdated metadata files
  to free disk space &ndash; see the section on cache trimming.)
  <br/><br/>
  A metadata file corresponding to a build rule is named by the rule hash and
  stores file names and content hashes of all artifacts produced by the rule.
  <br/><br/>
  A metadata file corresponding to a value-producing action is named by the
  action hash and stores the hash of the resulting value.
  <br/><br/>
  It is important to guarantee that rule and action hashes do not accidentally
  overlap, which may happen if one simply hashes their in-memory representations
  because a rule and an action might happen to be represented by the same
  sequence of bytes in memory.

* `root/files/v3` is a storage for artifacts, where files named by content
  hashes store the matching contents. We will create hard links to these files
  from build directories and rely on the hard link count, as well as on the last
  change time as useful metrics during cache trimming.

* `root/values/v3` is a storage for values. As in the case of `files`, we store
  the values in the files named by their content hashes. However, these files
  will always have the hard link count equal to 1, because they do not appear
  anywhere in build directories. By storing them in a separate directory, we
  simplify the job of the cache trimmer.

* `root/temp` contains temporary files used for atomic file operations needed
   when adding new entries to the cache, as will be described below.

Note that since this document was first written, some of the above paths have
changed due to version bumps (to `v4` and beyond).

## Adding entries to the cache

To add entries to the cache, we use the functions `store_artifacts` and
`store_value` described in the corresponding sections below. Setting possible
errors aside, these functions can succeed in two ways.

* They return `Stored` if the given entry is new and it has been successfully
  stored in the cache.

* They return `Already_present` if the given entry has already been present in
  the cache and can therefore be discarded. This is a rare scenario where
  multiple systems race to add the same entry, and only one of them will receive
  the glory of the `Stored` response.

### Atomic writing to the cache

As mentioned above, the cache can be modified concurrently by multiple systems,
so to prevent collisions on individual files, we need to create new files
atomically. To do that, we first create a temporary file in the `temp`
directory, then create a hard link to it from the cache (this operation will
fail if another process managed to create the cache entry earlier), and then
unlink the temporary file.

From now on, whenever we say "create a file", we mean create a file atomically.
If two systems attempt to create a file with the same name simultaneously, one
of them will win the competition and the contents it writes will remain in the
cache until it is deleted during cache trimming.

Note that it is possible for a metadata file with a given name to have multiple
possible contents due to _non-determinism_, and the cache implementation should
not assume otherwise.

### Storing artifacts

To store artifacts produced by a build rule, we perform the following sequence
of steps.

* Create a metadata file in the `meta` directory, listing all the artifacts.
  If the file already exists (which should be a rare case), verify that it
  contains the expected list of artifacts (both file names and content hashes).
  If it doesn't, we have found a non-deterministic build rule and report an
  error.

* For each artifact, we store it in the `files` directory using the artifact's
  content hash as the name. In each case, there are two scenarios:

    - If the artifact is already in the cache, we perform
      deduplication by replacing the artifact in the build directory
      with a hard link to the file stored in the cache. We assume that
      the build system will wait for `store_artifacts` to complete before
      starting any further actions that might read these artifacts from
      the build directory and thus interfere with the deduplication.

    - Otherwise, we create a hard link to the artifact from the `files`
      directory.

The function returns `Already_present` if the metadata file and all of the
artifacts were already in the cache; otherwise, it returns `Stored`.

### Storing values

Storing a value is simpler than storing artifacts because there is no need
for deduplication. The steps are:

* Create a metadata file in the `meta` directory, recording the value's hash.
  If the file already exists (which should be a rare case), verify that it
  contains the same hash. If it doesn't, we have found a non-deterministic build
  action and report an error.

* Store the value as a file in the `values` directory using the value's hash as
  the file name. If the file is already in the cache, we don't need to do
  anything.

The function returns `Already_present` if the metadata file and the value were
already in the cache; otherwise, it returns `Stored`.

## Restoring entries from the cache

To restore entries from the cache, we use the functions `restore_artifacts` and
`restore_value` described in the corresponding sections below. Setting possible
errors aside, these functions either fail to find the entry in the cache and
return `Not_found_in_cache`, or succeed and return `Restored` along with some
information about the restored entry.

### Restoring artifacts

Given a rule hash, the function `restore_artifacts` performs the following
steps.

* Look up the corresponding metadata file in the `meta` directory. If it doesn't
  exist, return `Not_found_in_cache`. Otherwise, read the list of artifacts,
  i.e. the list of file names and their content hashes from the metadata file.

* For each artifact, lookup the content hash in the `files` directory. If it
  doesn't exist, return `Not_found_in_cache`. Otherwise: (i) delete the
  corresponding (most likely stale) file in the build directory, and then (ii)
  create a hard link with the same name, pointing to the file in the cache.

If the above succeeds for every artifact in the list, the function returns
`Restored` along with the obtained list of file name and content hash pairs.

### Restoring values

Given an action hash, the function `restore_value` performs the following steps.

* Look up the corresponding metadata file in the `meta` directory. If it doesn't
  exist, return `Not_found_in_cache`. Otherwise, read the hash of the value.

* Look up the hash in the `values` directory. If it doesn't exist, return
  `Not_found_in_cache`. Otherwise, return `Restored` along with the value read
  from the stored file.

## Trimming the cache

Storing all historically produced artifacts and values is infeasible, so the
cache needs to be regularly trimmed. The current trimming algorithm performs the
following steps.

* Scan the `files` directory to find all currently unused artifact entries. An
  artifact is _unused_ if its hard link count is equal to 1. There is no point in
  trimming other entries, since they appear in at least one build directory. In
  fact, trimming them is potentially harmful because if the same entries were to
  be added to the cache again from a new directory, we would have been unable to
  perform the deduplication, thus losing some sharing opportunities.

* Scan the `values` directory to find all value entries. We have no information
  about their current usage, so we conservatively allow all of them to be
  trimmed and recomputed in the next build if needed.

* Sort the entries according to the following criteria:

    - Type: artifacts precede values in the trimming list since artifacts are
      generally larger and we know for sure that they are unused;

    - The time of last change: entries that became unused more recently go later
      in the list.

* Traverse the list and delete the corresponding entries until the trimming goal
  has been met. Right before deleting an artifact entry, double check that its
  hard link count is still equal to 1. A build system running concurrently might
  have created a hard link to it after we collected the information, so deleting
  this file from the cache could lead to a loss of sharing between different
  build directories.

* Finally, traverse the `meta` directory and remove all _broken_ metadata files,
  i.e. the files that refer to content hashes with no corresponding entries in
  the `files` and `values` directories. This step does not need to be done on
  every trimming. It is expensive but metadata files are generally small and
  there is no harm in keeping broken metadata files in the cache. In fact, the
  information contained in broken metadata files can be utilised by the build
  system for so-called _shallow builds_ where intermediate build artifacts are
  not materialised on the disk and it is sufficient to only know their hashes,
  which are listed in the metadata files.

To enable more sophisticated trimming strategies, we could augment the metadata
stored in the cache with information about the _cost_ of producing cache
entries, i.e. the time it would take to execute the corresponding rule or action
to restore the entry if needed. For deterministic build rules, we can do _local
cost reasoning_, i.e. we do not need to take the cost of rebuilding their
dependents into account, since such rebuilding would be unnecessary due to the
early cut-off optimisation.

Another promising idea is to add support for incremental cache trimming where
the build system informs the cache that a previously added entry has become
obsolete, letting the cache trim it early if it meets the trimming criteria.

### Interaction with the previous cache versions

Note also that as the new cache format evolves further and we, for example, move
from `files/v3` to `files/v4`, the cache trimmer will need to evolve too, to be
able to cope with entries of all currently supported versions.

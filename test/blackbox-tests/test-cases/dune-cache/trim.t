  $ export DUNE_CACHE=enabled
  $ export XDG_RUNTIME_DIR=$PWD/.xdg-runtime
  $ export XDG_CACHE_HOME=$PWD/.xdg-cache

  $ cat > dune-project <<EOF
  > (lang dune 2.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >   (targets target_a)
  >   (action (bash "touch beacon_a; echo target_a > target_a")))
  > (rule
  >   (targets target_b)
  >   (action (bash "sleep 0.1; touch beacon_b; echo target_b > target_b")))
  > (rule
  >   (targets non-exe)
  >   (action (bash "echo content > non-exe")))
  > (rule
  >   (targets exe)
  >   (action (bash "echo content > exe; chmod +x exe")))
  > (rule
  >   (targets multi_a multi_b)
  >   (action (bash "touch beacon_multi; echo multi_a > multi_a; echo multi_b > multi_b")))
  > EOF

Function to reset build tree and cache.

  $ reset ()
  > {
  >   rm -rf _build
  >   rm -rf $XDG_CACHE_HOME/dune
  > }

Check that trimming does not crash when the cache directory does not exist.

  $ dune cache trim --size 0B
  Freed 0B (0 files removed)

Check that the digest scheme for executable and non-executable digests hasn't
changed. If it has, make sure to increment the version of the cache. Note that
the current digests for both files match those computed by Jenga.

  $ dune build exe non-exe

  $ (cd "$PWD/.xdg-cache/dune/db/files/v4"; grep -rws . -e 'content' | sort)
  ./5e/5e5bb3a0ec0e689e19a59c3ee3d7fca8:content
  ./62/6274851067c88e9990e912be27cce386:content

Move all current entries to v3 and v4 to test trimming of old versions of cache.

  $ mkdir "$PWD/.xdg-cache/dune/db/files/v3"
  $ mkdir "$PWD/.xdg-cache/dune/db/meta/v3"
  $ mkdir "$PWD/.xdg-cache/dune/db/meta/v4"
  $ mv "$PWD/.xdg-cache/dune/db/files/v4"/* "$PWD/.xdg-cache/dune/db/files/v3"
  $ cp -r "$PWD/.xdg-cache/dune/db/meta/v5"/* "$PWD/.xdg-cache/dune/db/meta/v4"
  $ mv "$PWD/.xdg-cache/dune/db/meta/v5"/* "$PWD/.xdg-cache/dune/db/meta/v3"

Build some more targets.

  $ dune build target_a target_b

Dune stores the result of rule execution in a store keyed by "rule digests". If
the way such rule digests are computed changes, we could end up in a situation
where the same hash means something different before and after the change, which
is bad. To reduce the risk, we inject a version number into rule digests.

If you see the test below breaking, this means you changed the metadata format
or the way that digests are computed and you should increment the corresponding
version number. More specifically:

- If a digest value changed, you should increment the [rule_digest_version]
value in [build_system.ml].

- If the metadata format changed, you should increment the metadata version in
[layout.ml] in the [dune_cache_storage] library, e.g. from [meta/v5] to [meta/v6].
You will also need to make sure that the cache trimmer treats new and old cache
entries uniformly.

  $ (cd "$PWD/.xdg-cache/dune/db/meta/v5"; grep -rws . -e 'metadata' | sort ) > out
  $ cat out
  ./9e/9ec477cee96a2159fd639e0c0d70f065:((8:metadata)(5:files(8:target_a32:5637dd9730e430c7477f52d46de3909c)))
  ./cb/cbd5fd268a285fca51c14f9b818506fb:((8:metadata)(5:files(8:target_b32:8a53bfae3829b48866079fa7f2d97781)))

  $ digest="$(awk -F: '/target_b/ { digest=$1 } END { print digest }' < out)"

  $ dune_cmd stat size "$PWD/.xdg-cache/dune/db/meta/v5/$digest"
  70

Trimming the cache at this point should not remove any file entries because all
of them are still hard-linked from the build directory. However, we should trim
all metadata entries in [meta/v4] since they are broken: remember, we moved all
[files/v4] to [files/v3].

  $ find "$PWD/.xdg-cache/dune/db/meta/v4" -mindepth 2 -maxdepth 2 -type f | dune_cmd count-lines
  4
  $ dune cache trim --trimmed-size 1B
  Freed 287B (4 files removed)
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2
  $ find "$PWD/.xdg-cache/dune/db/meta/v4" -mindepth 2 -maxdepth 2 -type f | dune_cmd count-lines
  0

If we unlink a file in the build tree, then the corresponding file entry will be
trimmed.

  $ rm -f _build/default/target_a _build/default/beacon_a _build/default/beacon_b
  $ dune cache trim --trimmed-size 1B
  Freed 79B (2 files removed)
  $ dune build target_a target_b
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2
  $ dune_cmd exists _build/default/beacon_a
  true
  $ dune_cmd exists _build/default/beacon_b
  false

Now let's remove the remaining targets, left from the very first build and rerun
the trimmer. That will delete unused [files/v3] and the corresponding metadata
entries in [meta/v3].

  $ rm -rf _build
  $ find "$PWD/.xdg-cache/dune/db/files/v3" -mindepth 2 -maxdepth 2 -type f | dune_cmd count-lines
  4
  $ find "$PWD/.xdg-cache/dune/db/meta/v3" -mindepth 2 -maxdepth 2 -type f | dune_cmd count-lines
  4

We hide the output for reproducibility: some files are executable and their
sizes might vary on different platforms

  $ dune cache trim --size 0B > /dev/null

  $ find "$PWD/.xdg-cache/dune/db/files/v3" -mindepth 2 -maxdepth 2 -type f | dune_cmd count-lines
  0
  $ find "$PWD/.xdg-cache/dune/db/meta/v3" -mindepth 2 -maxdepth 2 -type f | dune_cmd count-lines
  0

The cache deletes oldest files first.

  $ reset
  $ dune build target_a target_b

To do so, we determine the creation time of the beacons and check which one is
older and which one is newer.

  $ BEACON_A_CTIME=$(dune_cmd stat creation _build/default/beacon_a)
  $ BEACON_B_CTIME=$(dune_cmd stat creation _build/default/beacon_b)
  $ echo "$BEACON_A_CTIME _build/default/beacon_a" > ages
  $ echo "$BEACON_B_CTIME _build/default/beacon_b" >> ages
  $ sort -n ages > beacons-by-age
  $ cat beacons-by-age
  $ OLDER=$(head -n1 beacons-by-age | awk '{print $2}')
  $ NEWER=$(tail -n1 beacons-by-age | awk '{print $2}')
  $ echo older $OLDER newer $NEWER
  $ rm -f _build/default/beacon_b _build/default/target_b
  $ dune_cmd wait-for-fs-clock-to-advance
  $ rm -f _build/default/beacon_a _build/default/target_a
  $ dune cache trim --trimmed-size 1B
  Freed 79B (2 files removed)
  $ dune build target_a target_b
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2
  $ dune_cmd exists $NEWER
  false
  $ dune_cmd exists $OLDER
  true

Test garbage collection: both [multi_a] and [multi_b] must be removed as they
are part of the same rule.

  $ reset
  $ dune build multi_a multi_b
  $ rm -f _build/default/multi_a _build/default/multi_b
  $ dune cache trim --trimmed-size 1B
  Freed 123B (2 files removed)

TODO: Test trimming priority in the [copy] mode. In PR #4497 we added a test but
it turned out to be flaky so we subsequently deleted it in #4511.

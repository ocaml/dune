  $ export DUNE_CACHE=enabled
  $ export XDG_CACHE_HOME=$PWD/.xdg-cache
  $ setup_xdg_runtime_dir

  $ make_dune_project 2.1
  $ cat > dune <<EOF
  > (rule
  >   (targets target_a)
  >   (action (bash "touch beacon_a; echo target_a > target_a")))
  > (rule
  >   (targets target_b)
  >   (action (bash "touch beacon_b; echo target_b > target_b")))
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

Check that the unit can be omitted for zero

  $ dune cache trim --size 0
  Freed 0B (0 files removed)

Check that the digest scheme for executable and non-executable digests hasn't
changed. If it has, make sure to increment the version of the cache. Note that
the current digests for both files match those computed by Jenga.

  $ dune build exe non-exe

  $ (cd "$PWD/.xdg-cache/dune/db/files/v4"; grep -rws . -e 'content' | sort | censor)
  ./5e/$DIGEST1:content
  ./e2/$DIGEST2:content

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

  $ metadata_dir="$PWD/.xdg-cache/dune/db/meta/v5"
  $ (cd "$metadata_dir"; find . -mindepth 2 -maxdepth 2 -type f | sort) > metadata-paths
  $ cat metadata-paths \
  > | dune_cmd subst-unique '[0-9a-f]{2}/[0-9a-f]{32}' '$DIGEST'
  ./$DIGEST1
  ./$DIGEST2

  $ while read path; do dune internal cache-metadata "$metadata_dir/$path"; done < metadata-paths \
  > | jq_dune -S -s 'sortCacheMetadataByFirstPath' \
  > | censor
  [
    {
      "files": [
        {
          "digest": "$DIGEST1",
          "path": "target_a"
        }
      ]
    },
    {
      "files": [
        {
          "digest": "$DIGEST2",
          "path": "target_b"
        }
      ]
    }
  ]

  $ digest="$(
  >   while read path; do
  >     if dune internal cache-metadata "$metadata_dir/$path" \
  >        | jq_dune -e 'cacheMetadataForPath("target_b")' >/dev/null
  >     then
  >       echo "$path"
  >     fi
  >   done < metadata-paths
  > )"

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

  $ dune clean _build/default/target_a _build/default/beacon_a _build/default/beacon_b
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

The [dune clean] commands below update the [ctime] of the corresponding cache
entries.
By deleting [target_b] first, we make its [ctime] older. The trimmer deletes
older entries first, which is why [target_b] is trimmed while [target_a] is not.
We know that [target_b] was trimmed, because it had to be rebuilt as indicated
by the existence of [beacon_b].

  $ dune clean _build/default/beacon_b _build/default/target_b
  $ dune_cmd wait-for-fs-clock-to-advance
  $ dune clean _build/default/beacon_a _build/default/target_a
  $ dune cache trim --trimmed-size 1B
  Freed 79B (2 files removed)
  $ dune build target_a target_b
  $ dune_cmd stat hardlinks _build/default/target_a
  2
  $ dune_cmd stat hardlinks _build/default/target_b
  2
  $ dune_cmd exists _build/default/beacon_a
  false
  $ dune_cmd exists _build/default/beacon_b
  true

Now let's redo the same test but delete the two targets in the opposite order,
thus making the trimmer delete [target_a] instead of [target_b] as above.

  $ reset
  $ dune build target_a target_b
  $ dune clean _build/default/beacon_a _build/default/target_a
  $ dune_cmd wait-for-fs-clock-to-advance
  $ dune clean _build/default/beacon_b _build/default/target_b
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

Test garbage collection: both [multi_a] and [multi_b] must be removed as they
are part of the same rule.

  $ reset
  $ dune build multi_a multi_b
  $ dune clean _build/default/multi_a _build/default/multi_b
  $ dune cache trim --trimmed-size 1B
  Freed 123B (2 files removed)

Test metadata for multiple file targets produced by the same rule.

  $ reset
  $ dune build multi_a multi_b
  $ metadata_dir="$PWD/.xdg-cache/dune/db/meta/v5"
  $ find "$metadata_dir" -mindepth 2 -maxdepth 2 -type f \
  > | while read path; do dune internal cache-metadata "$path"; done \
  > | jq_dune -S -s 'map(cacheMetadataWithPathPrefix("multi_")) | unique | .[]' \
  > | censor
  {
    "files": [
      {
        "digest": "$DIGEST1",
        "path": "multi_a"
      },
      {
        "digest": "$DIGEST2",
        "path": "multi_b"
      }
    ]
  }

A zero or negative file-trimming goal must still collect broken metadata,
without deleting unused artifacts. Directory-only metadata is not broken.

  $ reset
  $ cache="$PWD/.xdg-cache/dune/db"
  $ key=00000000000000000000000000000000
  $ for version in v3 v4; do
  >   mkdir -p "$cache/files/$version/00"
  >   printf artifact > "$cache/files/$version/00/$key"
  > done
  $ for version in v3 v4 v5; do
  >   mkdir -p "$cache/meta/$version/00"
  >   printf '((8:metadata)(5:files(3:dir5:<dir>)))' > "$cache/meta/$version/00/$key"
  >   printf broken > "$cache/meta/$version/00/00000000000000000000000000000001"
  > done
  $ dune cache trim --trimmed-size 0
  Freed 18B (3 files removed)
  $ dune cache trim --size 1MB
  Freed 0B (0 files removed)
  $ dune cache size --machine-readable
  16
  $ find "$cache/meta" -type f | dune_cmd count-lines
  3

Ignored filenames and shard directories must not contribute to the cache size.

  $ mkdir -p "$cache/files/v4/not-a-shard"
  $ printf ignored > "$cache/files/v4/not-a-shard/$key"
  $ printf ignored > "$cache/files/v4/00/not-a-digest"
  $ dune cache size --machine-readable
  16

Trimming more than the total unused size removes everything, including files
of size zero. Metadata containing only directories survives.

  $ touch "$cache/files/v4/00/00000000000000000000000000000002"
  $ dune cache trim --trimmed-size 1MB
  Freed 16B (3 files removed)
  $ find "$cache/meta" -type f | dune_cmd count-lines
  3
  $ dune cache size --machine-readable
  0

A goal equal to the unused byte total does not necessarily remove every file:
older zero-byte artifacts precede the last positive-size artifact, while newer
zero-byte artifacts survive after the goal is met.

  $ reset
  $ mkdir -p "$cache/files/v4/00"
  $ touch "$cache/files/v4/00/00000000000000000000000000000000"
  $ dune_cmd wait-for-fs-clock-to-advance
  $ printf artifact > "$cache/files/v4/00/00000000000000000000000000000001"
  $ dune_cmd wait-for-fs-clock-to-advance
  $ touch "$cache/files/v4/00/00000000000000000000000000000002"
  $ dune cache trim --trimmed-size 8B
  Freed 8B (2 files removed)
  $ find "$cache/files" -type f | dune_cmd count-lines
  1
  $ dune cache trim --trimmed-size 1B
  Freed 0B (1 files removed)

Check metadata validation, repeated references, and hexadecimal case across
all supported versions. Extra fields in the metadata header are supported.

  $ reset
  $ lower=abcdef0123456789abcdef0123456789
  $ upper=ABCDEF0123456789ABCDEF0123456789
  $ for version in v3 v4; do
  >   mkdir -p "$cache/files/$version/ab"
  >   printf artifact > "$cache/files/$version/ab/$lower"
  > done
  $ write_metadata () {
  >   for version in v3 v4 v5; do
  >     mkdir -p "$cache/meta/$version/00"
  >     name=$(printf '%032d' "$1")
  >     printf '%s' "$2" > "$cache/meta/$version/00/$name"
  >   done
  > }
  $ write_metadata 1 '((8:metadata)(5:files))'
  $ write_metadata 2 '((8:metadata)(5:files(3:dir5:<dir>)))'
  $ write_metadata 3 "((8:metadata)(5:files(1:a32:$lower)))"
  $ write_metadata 4 "((8:metadata)(5:files(1:a32:$lower)(1:b32:$lower)(3:dir5:<dir>)))"
  $ write_metadata 5 "((8:metadata(5:extra))(5:files(1:a32:$lower)))"
  $ write_metadata 6 "((8:metadata)(5:files(6:dir/up32:$upper)))"
  $ write_metadata 7 'broken'
  $ write_metadata 8 ''
  $ write_metadata 9 '((8:metadata)(5:files))()'
  $ write_metadata 10 '((8:metadata)(5:files(1:a3:abc)))'
  $ write_metadata 11 '((8:metadata)(5:files(1:a32:zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz)))'
  $ write_metadata 12 '((8:metadata)(5:files(1:a)))'
  $ write_metadata 13 '((8:not-meta)(5:files))'
  $ write_metadata 14 "((8:metadata)(5:files(1:a32:$lower(5:extra))))"
  $ write_metadata 15 "((8:metadata)(5:files(1:a32:$lower)(3:bad)))"
  $ find "$cache/meta" -type f | dune_cmd count-lines
  45
  $ dune cache trim --trimmed-size 0 > /dev/null
  $ find "$cache/meta" -type f | dune_cmd count-lines
  18

The next collection must notice references to artifacts deleted by trimming.

  $ dune cache trim --trimmed-size 1MB > /dev/null
  $ find "$cache/meta" -type f | dune_cmd count-lines
  6

Artifact inventories must keep file versions separate, including when the same
digest exists in both. Keep a live v3 artifact while evicting its unused v4 copy;
v3 metadata survives, but v4 and v5 metadata must be collected.

  $ reset
  $ for version in v3 v4; do
  >   mkdir -p "$cache/files/$version/00"
  >   printf artifact > "$cache/files/$version/00/$key"
  > done
  $ ln "$cache/files/v3/00/$key" live-artifact
  $ for version in v3 v4 v5; do
  >   mkdir -p "$cache/meta/$version/00"
  >   printf '((8:metadata)(5:files(1:a32:%s)))' "$key" > "$cache/meta/$version/00/$key"
  > done
  $ dune cache trim --size 0 > /dev/null
  $ for version in v3 v4 v5; do
  >   find "$cache/meta/$version" -type f | dune_cmd count-lines
  > done
  1
  0
  0

A later collection must notice that the previously live artifact is now unused.

  $ rm live-artifact
  $ dune cache trim --size 0 > /dev/null
  $ find "$cache/meta" -type f | dune_cmd count-lines
  0
  $ dune cache size --machine-readable
  0

Entry-name validation accepts both hexadecimal cases, but rejects wrong lengths
and non-hexadecimal characters. The two valid names remain distinct even on a
case-insensitive filesystem.

  $ reset
  $ mkdir -p "$cache/files/v4/ab"
  $ printf x > "$cache/files/v4/ab/abcdef0123456789abcdef0123456789"
  $ printf xx > "$cache/files/v4/ab/ABCDEF0123456789ABCDEF0123456788"
  $ printf ignored > "$cache/files/v4/ab/$(printf '%031d' 0)"
  $ printf ignored > "$cache/files/v4/ab/$(printf '%033d' 0)"
  $ printf ignored > "$cache/files/v4/ab/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
  $ printf ignored > "$cache/files/v4/ab/ABCDEF0123456789ABCDEF012345678G"
  $ dune cache size --machine-readable
  3
  $ dune cache trim --size 0
  Freed 3B (2 files removed)
  $ find "$cache/files/v4/ab" -type f | dune_cmd count-lines
  4

Test metadata for directory targets, which are stored with no digest.

  $ reset
  $ make_empty_child_directory_target_project
  $ dune build output
  $ metadata_dir="$PWD/.xdg-cache/dune/db/meta/v5"
  $ find "$metadata_dir" -mindepth 2 -maxdepth 2 -type f \
  > | while read path; do dune internal cache-metadata "$path"; done \
  > | jq_dune -S 'cacheMetadataForPath("output")' \
  > | censor
  {
    "files": [
      {
        "digest": null,
        "path": "output"
      },
      {
        "digest": "$DIGEST",
        "path": "output/file"
      },
      {
        "digest": null,
        "path": "output/child"
      }
    ]
  }

Trim to a nonzero size counts only unused artifacts across versions and evicts
the oldest first.

  $ reset
  $ old="$cache/files/v3/00/$key"
  $ recent="$cache/files/v4/11/11111111111111111111111111111111"
  $ pinned="$cache/files/v4/22/22222222222222222222222222222222"
  $ mkdir -p "$(dirname "$old")" "$(dirname "$recent")" "$(dirname "$pinned")"
  $ printf 1234 > "$old"
  $ dune_cmd wait-for-fs-clock-to-advance
  $ printf 123456 > "$recent"
  $ printf 12345678 > "$pinned"
  $ ln "$pinned" pinned
  $ dune cache size --machine-readable
  10

A limit at or above the current overhead leaves artifacts alone, but still
collects broken metadata.

  $ broken="$cache/meta/v5/00/$key"
  $ mkdir -p "$(dirname "$broken")"
  $ printf bad > "$broken"
  $ dune cache trim --size 12B
  Freed 3B (1 files removed)
  $ dune cache trim --size 10B
  Freed 0B (0 files removed)
  $ dune cache trim --size 6B
  Freed 4B (1 files removed)
  $ dune_cmd exists "$old"
  false
  $ dune_cmd exists "$recent"
  true
  $ dune cache size --machine-readable
  6
  $ dune_cmd stat hardlinks "$pinned"
  2


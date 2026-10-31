Source-backed targets do not cache content digests for source-tree files.

  $ echo '(lang dune 3.0)' > dune-project
  $ mkdir -p dir/sub
  $ printf x > invalid.txt
  $ printf y > stale.txt
  $ printf a > dir/a
  $ printf b > dir/sub/b
  $ touch -t 200001010000 invalid.txt
  $ dune build invalid.txt stale.txt dir/a dir/sub/b

The persistent digest database has no source-tree file entries after building
source-backed targets.

  $ dune internal digest-db dump > dump.out 2>&1
  $ grep -o 'entries = \[\]' dump.out
  entries = []

Modifying source files cannot produce invalid or stale source digest reports
because source files are tracked without content digests.

  $ printf z > invalid.txt
  $ touch -t 200001010000 invalid.txt
  $ printf yz > stale.txt
  $ dune internal digest-db check invalid.txt stale.txt
  []

External files are still content-digested, and the digest-db commands still
report cached and stale external entries.

  $ external_dir="$PWD/../external-dir"
  $ mkdir -p "$external_dir/sub"
  $ external_invalid="$external_dir/invalid.txt"
  $ external_stale="$external_dir/stale.txt"
  $ printf x > "$external_invalid"
  $ printf y > "$external_stale"
  $ printf a > "$external_dir/a"
  $ printf b > "$external_dir/sub/b"
  $ touch -t 200001010000 "$external_invalid"
  $ cat > dune <<EOF
  > (rule
  >  (deps
  >   $external_invalid
  >   $external_stale
  >   $external_dir/a
  >   $external_dir/sub/b)
  >  (target external-copy)
  >  (action (with-stdout-to external-copy (echo ok))))
  > EOF
  $ dune build external-copy

The full dump includes cached external entries:

  $ dune internal digest-db dump 2>&1 | grep -o 'external-dir/invalid.txt'
  external-dir/invalid.txt

The dump includes the other cached external entries too:

  $ dune internal digest-db dump 2>&1 | grep -o 'external-dir/a'
  external-dir/a
  $ dune internal digest-db dump 2>&1 | grep -o 'external-dir/sub/b'
  external-dir/sub/b

Modify one external file while preserving its mtime and another one normally.
Both become stale because their ctime changes.

  $ printf z > "$external_invalid"
  $ touch -t 200001010000 "$external_invalid"
  $ printf yz > "$external_stale"

Only actual digest mismatches are reported, classified as invalid or stale.

  $ dune internal digest-db check > external-check.out 2>&1 || true
  $ grep -o 'status = "[^"]*"' external-check.out
  status = "stale"
  status = "stale"

Future-dated entries are dropped before the digest database is saved. When this
happens, the saved [max_timestamp] must be recomputed and not retain the dropped
future timestamp.

  $ printf future > future.txt
  $ TZ=UTC touch -t 203001010000 future.txt
  $ dune build future.txt
  $ dune internal digest-db dump 2>&1 | grep 1893456000000000000
  [1]

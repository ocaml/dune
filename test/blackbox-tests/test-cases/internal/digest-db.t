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

  $ external="$PWD/../external.txt"
  $ printf external > "$external"
  $ cat > dune <<EOF
  > (rule
  >  (deps $external)
  >  (target external-copy)
  >  (action (copy $external external-copy)))
  > EOF
  $ dune build external-copy
  $ dune internal digest-db dump > external-dump.out 2>&1
  $ grep -A1 'External' external-dump.out
            External
              "$TESTCASE_ROOT/../external.txt"
  $ printf changed > "$external"
  $ dune internal digest-db check > external-check.out 2>&1 || true
  $ grep 'status = "stale"' external-check.out
  [ { status = "stale"
  $ grep -A1 'External' external-check.out
        External
          "$TESTCASE_ROOT/../external.txt"

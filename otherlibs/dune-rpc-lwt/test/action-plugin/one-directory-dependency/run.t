This test checks that executable that uses 'dynamic-run'
and depends on directory listing forces all targets in that
directory to be build.

  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ mkdir some_dir

  $ cat > some_dir/dune << EOF
  > (rule
  >  (target some_file1)
  >  (action (with-stdout-to %{target} (echo ""))))
  > \
  > (rule
  >  (target some_file2)
  >  (action (with-stdout-to %{target} (echo ""))))
  > \
  > (rule
  >  (target some_file3)
  >  (action (with-stdout-to %{target} (echo ""))))
  > EOF

  $ cp ./bin/foo.exe ./

  $ dune runtest
  dune
  some_file1
  some_file2
  some_file3

This is a bug: materialized subdirectories are returned in the listing, but
not tracked as dependencies. Adding another directory leaves a stale listing
until an unrelated input changes.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target listing)
  >  (deps trigger)
  >  (action (with-stdout-to listing (dynamic-run ./foo.exe))))
  > EOF
  $ echo first > trigger
  $ mkdir some_dir/nested
  $ echo payload > some_dir/nested/payload
  $ dune build some_dir/nested/payload
  $ dune build listing
  $ cat _build/default/listing
  dune
  nested
  some_file1
  some_file2
  some_file3
  $ mkdir some_dir/another
  $ echo payload > some_dir/another/payload
  $ dune build some_dir/another/payload
  $ dune build listing
  $ cat _build/default/listing
  dune
  nested
  some_file1
  some_file2
  some_file3
  $ echo second > trigger
  $ dune build listing
  $ cat _build/default/listing
  another
  dune
  nested
  some_file1
  some_file2
  some_file3

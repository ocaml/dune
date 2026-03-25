Test that we don't get pulled inside an infinite loop when 2 symlinks form a basic cycle.

  $ mkdir -p _src/dir_a
  $ mkdir -p _src/dir_b
  $ echo "file in a" > _src/dir_a/file_a.txt
  $ echo "file in b" > _src/dir_b/file_b.txt
  $ ln -s ../dir_b _src/dir_a/link_to_b
  $ ln -s ../dir_a _src/dir_b/link_to_a

  $ make_lockdir

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src)))
  > (build (run cat dir_a/file_a.txt))
  > EOF

  $ build_pkg foo 2>&1 | sed -E 's#/.*.sandbox/[^/]+#/.sandbox/$SANDBOX#g'
  File "_build/_private/default/.lock/dune.lock/foo.pkg", line 4, characters 7-150:
  4 |   (url file:/.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src)))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unable to resolve symlink dir_a/link_to_b, it is part of a cycle.
  File "_build/_private/default/.lock/dune.lock/foo.pkg", line 4, characters 7-150:
  4 |   (url file:/.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src)))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unable to resolve symlink dir_b/link_to_a, it is part of a cycle.
  [1]

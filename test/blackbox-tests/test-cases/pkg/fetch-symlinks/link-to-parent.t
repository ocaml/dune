Test that we don't get pulled inside an infinite loop when a link points to a parent of itself

Case 1: relative direct parent

  $ mkdir -p _src/mydir
  $ echo "content" > _src/mydir/file.txt
  $ ln -s .. _src/mydir/link_to_parent

  $ make_lockdir

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src)))
  > (build (run cat mydir/file.txt))
  > EOF

  $ tar czf _src.tar.gz _src

  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src.tar.gz)))
  > (build (run cat file.txt))
  > EOF

  $ build_pkg foo  2>&1 | sed -E 's#/.*.sandbox/[^/]+#/.sandbox/$SANDBOX#g' 
  File "_build/_private/default/.lock/dune.lock/foo.pkg", line 4, characters 7-150:
  4 |   (url file:/.sandbox/$SANDBOX/default/test/blackbox-tests/test-cases/pkg/fetch-symlinks/_src)))
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unable to resolve symlink mydir/link_to_parent, it is part of a cycle.
  [1]

This crashes the cram test infrastructure, saying "failed to delete sandbox in .../mydir/link_to_parent/mydir/link_to_parent/..."
$ build_pkg bar

Case 2: relative parent outside the source directory

  $ rm _src/mydir/link_to_parent
  $ ln -s ../.. _src/mydir/link_to_root


This error is correct, but not the one expected
  $ build_pkg foo
  Error: path outside the workspace: mydir/../.. from .
  [1]

$ build_pkg bar

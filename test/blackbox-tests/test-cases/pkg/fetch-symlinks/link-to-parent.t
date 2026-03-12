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

This fails correctly
  $ build_pkg foo 2>&1
  Error: Unable to resolve symlink mydir/link_to_parent, it is part of a cycle.
  [1]

This fails correctly
  $ build_pkg bar 2>&1 | sanitize_pkg_digest bar.0.0.1 | tail -3
  Error: Unable to resolve symlink
  _build/_private/default/.pkg/bar.0.0.1-DIGEST_HASH/source/mydir/link_to_parent,
  it is part of a cycle.
  [1]

Case 2: relative parent outside the source directory

  $ rm _src/mydir/link_to_parent
  $ ln -s ../.. _src/mydir/link_to_root


This fails correctly
  $ build_pkg foo
  Error: Unable to resolve symlink mydir/link_to_root: its target
  "$TESTCASE_ROOT"
  is outside the source directory
  [1]

This fails correctly
  $ build_pkg bar 2>&1 | sanitize_pkg_digest bar.0.0.1 | tail -3
  Error: Unable to resolve symlink
  _build/_private/default/.pkg/bar.0.0.1-DIGEST_HASH/source/mydir/link_to_parent,
  it is part of a cycle.
  [1]

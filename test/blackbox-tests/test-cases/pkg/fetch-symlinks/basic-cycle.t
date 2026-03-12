Test that we don't get pulled inside an infinite loop when 2 symlinks form a basic cycle.

  $ mkdir -p _src/dir_a
  $ mkdir -p _src/dir_b
  $ echo "file in a" > _src/dir_a/file_a.txt
  $ echo "file in b" > _src/dir_b/file_b.txt
  $ ln -s ../dir_b _src/dir_a/link_to_b
  $ ln -s ../dir_a _src/dir_b/link_to_a

  $ make_lockdir

  $ tar czf _src.tar.gz _src

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src.tar.gz)))
  > (build (run cat file.txt))
  > EOF

This fails correctly
  $ build_pkg foo 2>&1 | sanitize_pkg_digest foo.0.0.1 | tail -3
  Error: Unable to resolve symlink
  _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/source/dir_a/link_to_b/link_to_a,
  it is part of a cycle.
  [1]

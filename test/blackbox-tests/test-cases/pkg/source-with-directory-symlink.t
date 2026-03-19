Test that dune handles sources containing directory symlinks.

Currently, directory symlinks in sources cause failures. This could potentially
be improved by resolving the symlinks during fetch/extraction.

--------------------------------------------------------------------------------

Case 1: Local directory source containing a directory symlink.

  $ mkdir _src_local
  $ mkdir _src_local/real_dir
  $ echo "content" > _src_local/real_dir/file.txt
  $ ln -s real_dir _src_local/link_to_dir

  $ make_lockdir

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src_local)))
  > (build (run cat real_dir/file.txt))
  > EOF

CR-someday alizter: This fails because directory symlinks are not supported.
We could potentially resolve them during the copy.

  $ build_pkg foo 2>&1 | sanitize_pkg_digest foo.0.0.1
  Error: Is a directory
  -> required by
     _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/source/link_to_dir
  -> required by
     _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target
  [1]

Only the real directory was partially copied:

  $ ls _build/_private/default/.pkg/foo.*/source
  real_dir

--------------------------------------------------------------------------------

Case 2: Tarball source containing a directory symlink.

  $ mkdir _src_tar
  $ mkdir _src_tar/real_dir
  $ echo "content" > _src_tar/real_dir/file.txt
  $ ln -s real_dir _src_tar/link_to_dir
  $ tar czf _src.tar.gz _src_tar

  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url file://$PWD/_src.tar.gz)))
  > (build (run cat real_dir/file.txt))
  > EOF

CR-someday alizter: Tarball extraction preserves symlinks, but then the target
validation rejects directory symlinks. We could resolve them after extraction.

  $ build_pkg bar 2>&1 | sanitize_pkg_digest bar.0.0.1 | grep -E "^Error:|S_DIR"
  Error: Error trying to read targets after a rule was run:
  - default/.pkg/bar.0.0.1-DIGEST_HASH/source/link_to_dir: Unexpected file kind "S_DIR" (directory)
  [1]

The tarball was fully extracted (including the symlink):

  $ ls _build/_private/default/.pkg/bar.*/source
  link_to_dir
  real_dir

--------------------------------------------------------------------------------

Case 3: Downloaded tarball containing a directory symlink (with checksum).

  $ make_lockpkg baz <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url http://0.0.0.0:1)
  >   (checksum md5=$(md5sum $PWD/_src.tar.gz | cut -f1 -d' '))))
  > (build (run cat real_dir/file.txt))
  > EOF

  $ echo $PWD/_src.tar.gz >> fake-curls

CR-someday alizter: Same issue as Case 2, but the error occurs during checksum
validation which happens before the source is made available.

  $ build_pkg baz 2>&1 | sed 's/md5=[a-f0-9]*/md5=HASH/g' | grep -E "^Error:|S_DIR"
  Error: Error trying to read targets after a rule was run:
  - checksum/md5=HASH/dir/link_to_dir: Unexpected file kind "S_DIR" (directory)
  [1]

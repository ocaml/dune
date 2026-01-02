Test behavior with directory symlinks.

  $ make_lockdir

--------------------------------------------------------------------------------

Case 1: No .install file, directory symlink in %{lib}, engine rejects it.

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    mkdir -p %{lib}/%{pkg-self:name} \
  >    && printf 'content\n' > %{lib}/%{pkg-self:name}/file.txt \
  >    && mkdir -p %{lib}/%{pkg-self:name}/real_subdir \
  >    && ln -s real_subdir %{lib}/%{pkg-self:name}/subdir_link \
  >    && touch %{lib}/%{pkg-self:name}/META"))
  > EOF

CR-someday Alizter: Maybe in this case it actually makes sense to resolve the directory symlink somehow.

  $ build_pkg foo 2>&1 | sanitize_pkg_digest foo.0.0.1
  Error: Error trying to read targets after a rule was run:
  - default/.pkg/foo.0.0.1-DIGEST_HASH/target/lib/foo/subdir_link: Unexpected file kind "S_DIR" (directory)
  -> required by
     _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target
  [1]

--------------------------------------------------------------------------------

Case 2: .install file explicitly includes directory symlink.

CR-someday Alizter: What should happen in this case? Seems we didn't catch the
error correctly. Engine bug? Let's try to repro later.

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    mkdir -p real_subdir \
  >    && printf 'content\n' > real_subdir/file.txt \
  >    && ln -s real_subdir subdir_link \
  >    && printf 'lib: [\"real_subdir/file.txt\" \"subdir_link\"]\n' > foo.install"))
  > EOF

  $ build_pkg foo 2>&1 | sanitize_pkg_digest foo.0.0.1 | sed -E 's#\.sandbox/[^/]+#.sandbox/SANDBOX#g'
  Error:
  link(_build/.sandbox/SANDBOX/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target/lib/foo/subdir_link): Operation not permitted
  -> required by
     _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target
  [1]

--------------------------------------------------------------------------------

Case 3: .install file excludes the directory symlink.

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    mkdir -p real_subdir \
  >    && printf 'content\n' > real_subdir/file.txt \
  >    && ln -s file.txt real_subdir/link.txt \
  >    && ln -s real_subdir subdir_link \
  >    && printf 'lib: [\"real_subdir/file.txt\" \"real_subdir/link.txt\"]\n' > foo.install"))
  > EOF

  $ build_pkg foo

The directory symlink is not in the installed targets, so the engine does not
reject it. Only the real directory contents are installed:

  $ ls $(get_build_pkg_dir foo)/target/lib/foo
  file.txt
  link.txt

The file symlink is converted to a hardlink:

  $ dune_cmd stat kind $(get_build_pkg_dir foo)/target/lib/foo/link.txt
  regular file

  $ dune_cmd stat hardlinks $(get_build_pkg_dir foo)/target/lib/foo/link.txt
  2

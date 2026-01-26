Test that broken symlinks are detected during symlink resolution.

  $ make_lockdir

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    mkdir -p %{lib}/%{pkg-self:name} \
  >    && ln -s nonexistent.txt %{lib}/%{pkg-self:name}/broken.txt \
  >    && touch %{lib}/%{pkg-self:name}/META"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends foo))
  > EOF

The broken symlink is detected:

  $ build_pkg foo 2>&1 | sanitize_pkg_digest foo.0.0.1 \
  > | dune_cmd subst '\.sandbox/[a-f0-9]+' '.sandbox/$SANDBOX'
  Error:
  readlink(_build/.sandbox/$SANDBOX/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target/lib/foo/nonexistent.txt): No such file or directory
  -> required by
     _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target
  [1]

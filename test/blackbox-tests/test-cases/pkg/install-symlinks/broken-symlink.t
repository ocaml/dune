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

  $ make_package_project 3.22 foo

The broken symlink is detected:

  $ build_pkg foo 2>&1 | censor
  Error:
  readlink(_build/.sandbox/$DIGEST1/_private/default/.pkg/foo.0.0.1-$DIGEST2/target/lib/foo/nonexistent.txt): No such file or directory
  -> required by
     _build/_private/default/.pkg/foo.0.0.1-$DIGEST2/target
  [1]

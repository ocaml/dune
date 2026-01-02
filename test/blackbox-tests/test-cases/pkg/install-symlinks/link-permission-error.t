Test that link errors cause the build to fail.

If the directory containing the symlink is made non-writable, unlinking the
symlink before hardlinking will fail with EACCES.

  $ make_lockdir

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    mkdir -p %{lib}/%{pkg-self:name} \
  >    && printf 'real content\n' > %{lib}/%{pkg-self:name}/real.txt \
  >    && ln -s real.txt %{lib}/%{pkg-self:name}/link.txt \
  >    && touch %{lib}/%{pkg-self:name}/META \
  >    && chmod -w %{lib}/%{pkg-self:name}"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends foo))
  > EOF

Build fails with a symlink resolution error:

  $ build_pkg foo 2>&1 | sanitize_pkg_digest foo.0.0.1 | dune_cmd subst '\.sandbox/[^/]+' '.sandbox/SANDBOX'
  Error: failed to delete sandbox in
  _build/.sandbox/SANDBOX
  Reason:
  rmdir(_build/.sandbox/SANDBOX/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target/lib/foo): Directory not empty
  -> required by
     _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target
  Error:
  unlink(_build/.sandbox/SANDBOX/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target/lib/foo/link.txt): Permission denied
  -> required by
     _build/_private/default/.pkg/foo.0.0.1-DIGEST_HASH/target
  [1]

Restore permissions for cleanup:

  $ chmod -R +w _build 2>/dev/null; rm -rf _build

Test symlinks using absolute paths that point inside the target directory.
Since we use realpath, these resolve correctly and work with caching.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/_cache

  $ make_lockdir

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    true BUILDING_FOO_PACKAGE \
  >    && mkdir -p %{lib}/%{pkg-self:name} \
  >    && printf 'real content\n' > %{lib}/%{pkg-self:name}/real.txt \
  >    && ln -s \$(realpath %{lib}/%{pkg-self:name}/real.txt) %{lib}/%{pkg-self:name}/link.txt \
  >    && touch %{lib}/%{pkg-self:name}/META"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends foo))
  > EOF

  $ build_pkg foo

Verify the build ran:

  $ count_trace BUILDING_FOO_PACKAGE
  1

The symlink is resolved to a regular file:

  $ dune_cmd stat kind $(get_build_pkg_dir foo)/target/lib/foo/link.txt
  regular file

Both files are hardlinked:

  $ dune_cmd stat hardlinks $(get_build_pkg_dir foo)/target/lib/foo/real.txt
  3
  $ dune_cmd stat hardlinks $(get_build_pkg_dir foo)/target/lib/foo/link.txt
  3

Clean and rebuild to verify cache restore:

  $ rm -rf _build
  $ build_pkg foo

  $ count_trace BUILDING_FOO_PACKAGE
  0

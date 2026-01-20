Test installing files from inside a symlinked directory.
The source has a symlink to a directory, we install files from inside it (via the symlink),
but don't install the directory symlink itself.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/_cache

  $ make_lockdir

Create a source with a directory symlink. We install files through the symlink
but not the symlink itself. Use cp -P to preserve symlinks so our resolution
code is exercised:

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    true BUILDING_FOO_PACKAGE \
  >    && mkdir -p real_dir \
  >    && printf 'real content\n' > real_dir/real.txt \
  >    && ln -s real.txt real_dir/link.txt \
  >    && ln -s real_dir link_to_dir \
  >    && mkdir -p %{lib}/%{pkg-self:name} \
  >    && cp link_to_dir/real.txt %{lib}/%{pkg-self:name}/ \
  >    && cp -P link_to_dir/link.txt %{lib}/%{pkg-self:name}/ \
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

  $ count_trace BUILDING_FOO_PACKAGE
  1

The symlink is resolved by dune's symlink resolution code:

  $ dune_cmd stat kind $(get_build_pkg_dir foo)/target/lib/foo/link.txt
  regular file

  $ dune_cmd stat hardlinks $(get_build_pkg_dir foo)/target/lib/foo/real.txt
  3

Caching works:

  $ rm -rf _build
  $ build_pkg foo
  $ count_trace BUILDING_FOO_PACKAGE
  0

Test behavior when a package creates nested symlinks (symlink pointing to symlink).

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/_cache

  $ make_lockdir

Create a package with nested symlinks: link2 -> link1 -> real.txt

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    true BUILDING_FOO_PACKAGE \
  >    && mkdir -p %{lib}/%{pkg-self:name} \
  >    && printf 'real content\n' > %{lib}/%{pkg-self:name}/real.txt \
  >    && ln -s real.txt %{lib}/%{pkg-self:name}/link1.txt \
  >    && ln -s link1.txt %{lib}/%{pkg-self:name}/link2.txt \
  >    && touch %{lib}/%{pkg-self:name}/META"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends foo))
  > EOF

Build the package:

  $ build_pkg foo

Verify the build ran:

  $ count_trace BUILDING_FOO_PACKAGE
  1

Check that the files exist:

  $ ls $(get_build_pkg_dir foo)/target/lib/foo | sort
  META
  link1.txt
  link2.txt
  real.txt

All symlinks should be resolved to regular files (hardlinks):

  $ dune_cmd stat kind $(get_build_pkg_dir foo)/target/lib/foo/real.txt
  regular file
  $ dune_cmd stat kind $(get_build_pkg_dir foo)/target/lib/foo/link1.txt
  regular file
  $ dune_cmd stat kind $(get_build_pkg_dir foo)/target/lib/foo/link2.txt
  regular file

All should have the same content:

  $ cat $(get_build_pkg_dir foo)/target/lib/foo/link1.txt
  real content
  $ cat $(get_build_pkg_dir foo)/target/lib/foo/link2.txt
  real content

Check hardlink counts - all three should be hardlinked to the same inode:

  $ dune_cmd stat hardlinks $(get_build_pkg_dir foo)/target/lib/foo/real.txt
  4

Clean and rebuild to verify cache restore:

  $ rm -rf _build
  $ build_pkg foo

The build command should not be rerun since it will be restored from cache:

  $ count_trace BUILDING_FOO_PACKAGE
  0

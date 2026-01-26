Test behavior when a symlink chain goes through a file outside the target directory.
e.g., link.txt -> $PWD/intermediate.txt -> real.txt (where intermediate is outside)

The chain goes through an outside path, but the final target is inside target_dir.
This case is pathological, but we want to exercise our symlink resolution code.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/_cache

  $ make_lockdir

Create a directory outside the build for the intermediate symlink:

  $ mkdir outside_dir

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    mkdir -p %{lib}/%{pkg-self:name} \
  >    && printf 'real content\n' > %{lib}/%{pkg-self:name}/real.txt \
  >    && ln -sf \$(realpath %{lib}/%{pkg-self:name}/real.txt) $PWD/outside_dir/intermediate.txt \
  >    && ln -s $PWD/outside_dir/intermediate.txt %{lib}/%{pkg-self:name}/link.txt \
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

The symlink should be resolved to a regular file:

  $ dune_cmd stat kind $(get_build_pkg_dir foo)/target/lib/foo/link.txt
  regular file

link.txt and real.txt should be hardlinks to the same file (hardlink count > 1):

  $ dune_cmd stat hardlinks $(get_build_pkg_dir foo)/target/lib/foo/link.txt
  3
  $ dune_cmd stat hardlinks $(get_build_pkg_dir foo)/target/lib/foo/real.txt
  3

The outside intermediate has only 1 hardlink (it's still just a symlink):

  $ dune_cmd stat hardlinks outside_dir/intermediate.txt
  1

  $ cat $(get_build_pkg_dir foo)/target/lib/foo/link.txt
  real content

Caching works - the symlink was resolved so it can be cached:

  $ rm -rf _build
  $ build_pkg foo
  $ dune_cmd stat hardlinks $(get_build_pkg_dir foo)/target/lib/foo/link.txt
  3

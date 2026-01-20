Test that packages with symlinks in their install output are properly cached.
Dune resolves symlinks to hardlinks so that the cache can store them.

The compiler is one such package that has this kind of layout.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/_cache

  $ make_lockdir

Create a package that installs both a regular file and a symlink to it.
This is similar to how compilers install binaries (e.g., ocamlc -> ocamlc.opt).

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    true BUILDING_FOO_PACKAGE \
  >    && mkdir -p %{lib}/%{pkg-self:name} \
  >    && printf 'real content\n' > %{lib}/%{pkg-self:name}/real.txt \
  >    && ln -s real.txt %{lib}/%{pkg-self:name}/link.txt \
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

Verify the build ran by checking the trace for our marker command:

  $ count_trace BUILDING_FOO_PACKAGE
  1

Check that the files exist:

  $ ls $(get_build_pkg_dir foo)/target/lib/foo | sort
  META
  link.txt
  real.txt

The symlink has been resolved to a regular file (hardlink to the target):

  $ dune_cmd stat kind $(get_build_pkg_dir foo)/target/lib/foo/link.txt
  regular file

Check hardlink count. Files should have hardlinks > 1 indicating they are cached.
real.txt has 3 hardlinks: original, link.txt (resolved), and cache entry.

  $ dune_cmd stat hardlinks $(get_build_pkg_dir foo)/target/lib/foo/real.txt
  3
  $ dune_cmd stat hardlinks $(get_build_pkg_dir foo)/target/lib/foo/META
  2

Clean and rebuild to verify cache restore:

  $ rm -rf _build
  $ build_pkg foo

The build command should not be rerun since it will be restored from cache.

  $ count_trace BUILDING_FOO_PACKAGE
  0


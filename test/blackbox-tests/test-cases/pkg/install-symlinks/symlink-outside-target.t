Test symlinks pointing to files outside the target directory.

CR-someday Alizter: This behavior is questionable. We convert the symlink to a
hardlink pointing to an external file, which ties the build output to external
content. If the external file changes, the target would be stale. Consider
whether we should leave such symlinks as-is, copy the file, or error instead.

  $ export DUNE_CACHE=enabled
  $ export DUNE_CACHE_ROOT=$PWD/_cache

  $ make_lockdir
  $ mkdir outside_dir
  $ echo 'outside content' > outside_dir/external_file.txt

Create a package with a symlink pointing outside target:

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\
  >    true BUILDING_FOO_PACKAGE \
  >    && mkdir -p %{lib}/%{pkg-self:name} \
  >    && ln -s $PWD/outside_dir/external_file.txt %{lib}/%{pkg-self:name}/external.txt \
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

The symlink is resolved to a hardlink to the external file:

  $ dune_cmd stat kind $(get_build_pkg_dir foo)/target/lib/foo/external.txt
  regular file

The content is accessible:

  $ cat $(get_build_pkg_dir foo)/target/lib/foo/external.txt
  outside content

Since the symlink was resolved to a hardlink, caching works:

  $ rm -rf _build
  $ build_pkg foo
  $ count_trace BUILDING_FOO_PACKAGE
  0

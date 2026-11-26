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

  $ make_package_project 3.22 foo

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

An install command copies a dependency's file without dereferencing symlinks.
The destination is at a different directory depth than the dependency file.

  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (install (system "echo payload > %{pkg-self:bin}/marker"))
  > EOF
  $ make_lockpkg consumer <<'EOF'
  > (version 0.0.1)
  > (depends provider)
  > (install
  >  (system "cp -P %{pkg:provider:bin}/marker %{pkg-self:share}/marker"))
  > EOF
  $ cat > symlink-config <<'EOF'
  > (lang dune 3.25)
  > (sandboxing_preference none symlink)
  > EOF
  $ pkg_dir=$(get_build_pkg_dir consumer)
  $ if dune build --cache=disabled --config-file symlink-config \
  >   "$pkg_dir/target" > install.log 2>&1; then
  >   cat "$pkg_dir/target/share/consumer/marker"
  > else
  >   dune_cmd subst 'readlink\([^)]*\)' 'readlink(...)' < install.log | censor
  >   false
  > fi
  Error:
  readlink(...): No such file or directory
  -> required by _build/_private/default/.pkg/consumer/target
  [1]

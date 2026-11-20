Applying patches

  $ mkdir test-source
  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (progn
  >   (patch foo.patch)
  >   (system "cat foo.ml")))
  > EOF

  $ make_lockpkg_file test foo.patch <<EOF
  > diff --git a/foo.ml b/foo.ml
  > new file mode 100644
  > index 0000000..557db03
  > --- /dev/null
  > +++ b/foo.ml
  > @@ -0,0 +1 @@
  > +Hello World
  > EOF

  $ build_pkg test
  Hello World

Ed-format patches are not supported and should give a clear error:

  $ mkdir ed-test-source
  $ cat > ed-test-source/foo.ml <<EOF
  > original content
  > EOF
  $ make_lockpkg ed-test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/ed-test-source))
  > (build
  >  (progn
  >   (patch foo.patch)
  >   (system "cat foo.ml")))
  > EOF

  $ make_lockpkg_file ed-test foo.patch <<EOF
  > 1c
  > new content
  > .
  > EOF

  $ build_pkg ed-test 2>&1 | censor
  Error: Could not parse the patch file. Only unified diff format is supported.
  Context diffs and ed commands are not supported.
  -> required by
     _build/_private/default/.pkg/ed-test.0.0.1-$DIGEST/target
  [1]

Demonstrate that the original source shouldn't be modified:

  $ cat _build/_private/default/.pkg/test/source/foo.ml
  cat: _build/_private/default/.pkg/test/source/foo.ml: No such file or directory
  [1]

Patch action without a source stanza should give an informative error:

  $ make_lockpkg no-source <<EOF
  > (version 0.0.1)
  > (build
  >  (progn
  >   (patch foo.patch)
  >   (system "cat foo.ml")))
  > EOF

  $ make_lockpkg_file no-source foo.patch <<EOF
  > diff --git a/foo.ml b/foo.ml
  > index 557db03..a1fc8d2 100644
  > --- a/foo.ml
  > +++ b/foo.ml
  > @@ -1 +1 @@
  > -Hello World
  > +Patched World
  > EOF

CR-someday Alizter: This error should be more helpful, it currently tells us
what has failed but not why and how to fix it (stop patching an opam file
without sources).

  $ build_pkg no-source 2>&1 | censor
  Error: Cannot edit file "foo.ml": file does not exist
  -> required by
     _build/_private/default/.pkg/no-source.0.0.1-$DIGEST/target
  [1]

Empty patches are treated as a no-op, matching Opam behavior:

  $ mkdir empty-test-source
  $ cat > empty-test-source/foo.ml <<EOF
  > original content
  > EOF
  $ make_lockpkg empty-test <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/empty-test-source))
  > (build
  >  (progn
  >   (patch foo.patch)
  >   (system "cat foo.ml")))
  > EOF

  $ make_lockpkg_file empty-test foo.patch <<EOF
  > EOF

  $ build_pkg empty-test
  original content

A package can edit its own sources in place. Even when hardlink sandboxing
is preferred, Dune copies both the sources and the installed outputs of the
dependencies.

  $ make_lockpkg dep <<'EOF'
  > (version 0.0.1)
  > (install (system "echo dependency > %{pkg-self:bin}/marker"))
  > EOF
  $ mkdir -p edit-source/nested
  $ echo original > edit-source/nested/input
  $ make_lockpkg edit-test <<EOF
  > (version 0.0.1)
  > (depends dep)
  > (source (copy $PWD/edit-source))
  > (build
  >  (system
  >   "\| dune_cmd stat kind %{pkg:dep:bin}/marker
  >   "\| dune_cmd stat hardlinks %{pkg:dep:bin}/marker
  >   "\| dune_cmd stat kind nested/input
  >   "\| dune_cmd stat hardlinks nested/input
  >   "\| echo edited > nested/input
  >   "\| cat nested/input
  >  ))
  > EOF
  $ cat > hardlink-config <<'EOF'
  > (lang dune 3.25)
  > (sandboxing_preference none hardlink)
  > EOF
  $ pkg_dir=$(get_build_pkg_dir edit-test)
  $ dune build --cache=disabled --config-file hardlink-config \
  >   "$pkg_dir/target"
  regular file
  1
  regular file
  1
  edited
  $ cat "$pkg_dir/source/nested/input"
  original

The dependency and source file are also copied when symlink sandboxing is
preferred. Editing the source still leaves the staged source intact.
Skip this case on Windows, where Dune does not support symlink sandboxing.

  $ cat > symlink-config <<'EOF'
  > (lang dune 3.25)
  > (sandboxing_preference none symlink)
  > EOF
  $ cat > symlink.expected <<'EOF'
  > regular file
  > 1
  > regular file
  > 1
  > edited
  > original
  > EOF
  $ os_type=$(ocamlc -config-var os_type)
  $ if [ "$os_type" != Win32 ]; then
  >   rm -r "$pkg_dir/target" &&
  >   dune build --cache=disabled --config-file symlink-config \
  >     "$pkg_dir/target" > symlink.out 2>&1 &&
  >   cat "$pkg_dir/source/nested/input" >> symlink.out &&
  >   diff symlink.expected symlink.out
  > fi

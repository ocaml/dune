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
  Error: No patches in patch file detected
  -> required by
     _build/_private/default/.pkg/ed-test.0.0.1-$DIGEST/target
  [1]

Demonstrate that the original source shouldn't be modified:

  $ cat _build/_private/default/.pkg/test/source/foo.ml
  cat: _build/_private/default/.pkg/test/source/foo.ml: No such file or directory
  [1]

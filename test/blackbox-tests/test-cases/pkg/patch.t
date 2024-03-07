Applying patches

  $ . ./helpers.sh

  $ mkdir test-source
  $ make_lockdir
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/test-source))
  > (build
  >  (progn
  >   (patch foo.patch)
  >   (system "cat foo.ml")))
  > EOF

  $ mkdir dune.lock/test.files
  $ cat >dune.lock/test.files/foo.patch <<EOF
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

Demonstrate that the original source shouldn't be modified:

  $ cat _build/_private/default/.pkg/test/source/foo.ml
  cat: _build/_private/default/.pkg/test/source/foo.ml: No such file or directory
  [1]

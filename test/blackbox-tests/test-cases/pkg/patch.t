Applying patches

  $ mkdir test-source
  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test <<EOF
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

  $ dune build .pkg/test/target/
  patching file foo.ml
  Hello World

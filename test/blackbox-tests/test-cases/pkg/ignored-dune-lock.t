Test that shows what happens when .dune-solution-cache is ignored.

  $ . ./helpers.sh

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (build
  >  (progn
  >   (run touch foo.ml)
  >   (patch foo.patch)))
  > EOF
  $ make_lockpkg_file test foo.patch <<EOF
  > diff --git a/foo.ml b/foo.ml
  > index b69a69a5a..ea988f6bd 100644
  > --- a/foo.ml
  > +++ b/foo.ml
  > @@ -0,0 +1 @@
  > +let () = print_endline "Hello, World!"
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (executable
  >  (name foo))
  > EOF
  $ cat > src/foo.ml <<EOF
  > let () = ()
  > EOF
  > cat > dune-project <<EOF
  > (lang dune 3.16)
  > EOF

Building test works when the .dune-solution-cache is visible to dune.

  $ build_pkg test

Now the project is changed to only include src (which effectively ignores
.dune-solution-cache):

  $ cat > dune <<EOF
  > (dirs src)
  > EOF

Building fails as the patch cannot be found anymore

  $ build_pkg test
  Error: Lock directory is not active for context "default".
  [1]

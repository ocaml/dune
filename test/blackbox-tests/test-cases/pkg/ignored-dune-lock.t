Test that shows what happens when dune.lock is ignored.

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

Building test works when the dune.lock is visible to dune.

  $ build_pkg test

Now the project is changed to only include src (which effectively ignores
dune.lock):

  $ cat > dune <<EOF
  > (dirs src)
  > EOF

Building fails as the patch cannot be found anymore

  $ build_pkg test 2>&1 | sed 's|\.sandbox/[a-f0-9]*/|.sandbox/<hash>/|'
  Error:
  open(_build/.sandbox/<hash>/_private/default/.pkg/test/source/foo.patch): No such file or directory
  -> required by _build/_private/default/.pkg/test/target

And the backage cannot be shown:

  $ show_pkg test
  

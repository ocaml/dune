Test that shows what happens when dune.lock is ignored.

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

Forcing a sandbox mode also applies it to the rule that stages dune.lock.
That rule currently depends directly on files in the source tree.

  $ dune build --sandbox=copy "$(get_build_pkg_dir test)/target" > build.log 2>&1
  [1]
  $ grep -F "we don't depend on paths from the source" build.log
    ("we don't depend on paths from the source", {})
  $ grep -Fq '_private/default/.lock/dune.lock' build.log

Building test works when the dune.lock is visible to dune.

  $ build_pkg test

Now the project is changed to only include src (which effectively ignores
dune.lock):

  $ cat > dune <<EOF
  > (dirs src)
  > EOF

Building fails as the patch cannot be found anymore

  $ build_pkg test
  Error: Don't know how to build _build/_private/default/.pkg/test/target
  [1]

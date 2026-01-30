Demonstrate that re_exports is incorrectly triggering the unused library check:

  $ make_dune_project 3.21

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (modules foo))
  > (library
  >  (name bar)
  >  (modules bar)
  >  (libraries (re_export foo)))
  > EOF

  $ touch foo.ml
  $ cat >bar.ml <<EOF
  > let x = ()
  > EOF

  $ mkdir use
  $ cat >use/dune <<EOF
  > (executable
  >  (libraries bar)
  >  (name use))
  > EOF

  $ cat >use/use.ml <<EOF
  > let () = Bar.x
  > EOF

  $ dune build @use/unused-libs

Demonstrate that re_exports is incorrectly triggering the unused library check:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

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

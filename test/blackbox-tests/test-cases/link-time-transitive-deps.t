Link time code generation should work with implicit transitive deps

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (implicit_transitive_deps false)
  > EOF

  $ touch foo.ml bar.ml
  $ cat >dune <<EOF
  > (library
  >  (libraries dune-build-info)
  >  (modules foo)
  >  (name foo))
  > (executable
  >  (modules bar)
  >  (libraries foo)
  >  (name bar))
  > EOF

  $ dune build ./bar.exe

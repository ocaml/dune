 Temporary special merlin support for melange only libs

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ lib=foo
  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (modes melange))
  > EOF

  $ touch bar.ml $lib.ml
  $ dune build @check
  $ dune ocaml-merlin --dump-config="$(pwd)" | grep -i "$lib"
  Foo
    $TESTCASE_ROOT/_build/default/.foo.objs/melange)
     Foo__
    $TESTCASE_ROOT/_build/default/.foo.objs/melange)
     Foo__
  Foo__
    $TESTCASE_ROOT/_build/default/.foo.objs/melange)
     Foo__

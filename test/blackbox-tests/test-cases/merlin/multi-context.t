Showcase dune ocaml merlin behavior with libraries built in the non-default context

  $ cat >dune-project <<EOF
  > (lang dune 3.14)
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.14)
  > 
  > (context default)
  > 
  > (context
  >  (default
  >   (name alt)))
  > EOF

  $ lib=foo
  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (enabled_if (= %{context_name} "default")))
  > EOF

  $ touch bar.ml $lib.ml
  $ dune build @check
  $ dune ocaml merlin dump-config "$PWD" | grep -i "$lib"
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (FLG (-open Foo__))
  Foo: _build/default/foo
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)
   (FLG (-open Foo__))
  Foo__: _build/default/foo__
    $TESTCASE_ROOT/_build/default/.foo.objs/byte)

Now try with non-default context

  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (enabled_if (= %{context_name} "alt")))
  > EOF
  $ dune build @check
  $ dune ocaml merlin dump-config "$PWD" | grep -i "$lib"
  [1]

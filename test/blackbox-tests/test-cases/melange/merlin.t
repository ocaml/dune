 Temporary special merlin support for melange only libs

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ lib=foo
  $ mkdir $lib
  $ cat >$lib/dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (flags -bs-jsx 3)
  >  (modes melange))
  > EOF

  $ touch $lib/bar.ml $lib/$lib.ml
  $ dune build @check

All library entries contain a ppx directive

  $ dune ocaml-merlin --dump-config="$(pwd)/$lib"
  Foo
  ((STDLIB /home/me/code/dune/_opam/lib/melange)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/foo/.foo.objs/melange)
   (S
    $TESTCASE_ROOT/foo)
   (FLG (-ppx melc -as-ppx -open Foo__ -bs-jsx 3))
   (FLG (-open Foo__ -bs-jsx 3)))
  Bar
  ((STDLIB /home/me/code/dune/_opam/lib/melange)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/foo/.foo.objs/melange)
   (S
    $TESTCASE_ROOT/foo)
   (FLG (-ppx melc -as-ppx -open Foo__ -bs-jsx 3))
   (FLG (-open Foo__ -bs-jsx 3)))
  Foo__
  ((STDLIB /home/me/code/dune/_opam/lib/melange)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/foo/.foo.objs/melange)
   (S
    $TESTCASE_ROOT/foo)
   (FLG (-ppx melc -as-ppx -open Foo__ -bs-jsx 3))
   (FLG (-open Foo__ -bs-jsx 3)))

  $ target=output
  $ cat >dune <<EOF
  > (melange.emit
  >  (target "$target")
  >  (entries main)
  >  (flags -foo bar)
  >  (module_system commonjs))
  > EOF

  $ touch main.ml
  $ dune build @check
  $ dune ocaml-merlin --dump-config="$(pwd)" | grep -i "$target"
    $TESTCASE_ROOT/_build/default/.output.mobjs/melange)

The melange.emit entry contains a ppx directive, but no -bs-jsx flag

  $ dune ocaml-merlin --dump-config="$(pwd)"
  Main
  ((STDLIB /home/me/code/dune/_opam/lib/melange)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.output.mobjs/melange)
   (S
    $TESTCASE_ROOT)
   (FLG (-ppx melc -as-ppx -foo bar))
   (FLG (-foo bar)))

  $ $JBUILDER build @print-merlins -j1 --display short --root .
      ocamldep sanitize-dot-merlin/sanitize_dot_merlin.ml.d
        ocamlc sanitize-dot-merlin/.sanitize_dot_merlin.eobjs/sanitize_dot_merlin.{cmi,cmo,cmt}
      ocamlopt sanitize-dot-merlin/.sanitize_dot_merlin.eobjs/sanitize_dot_merlin.{cmx,o}
      ocamlopt sanitize-dot-merlin/sanitize_dot_merlin.exe
  sanitize_dot_merlin alias print-merlins
  # Processing exe/.merlin
  B ../_build/default/exe/.x.eobjs
  B ../_build/default/lib/.foo.objs
  B $LIB_PREFIX/lib/bytes
  B $LIB_PREFIX/lib/findlib
  B $LIB_PREFIX/lib/ocaml
  FLG -w -40
  S .
  S ../lib
  S $LIB_PREFIX/lib/bytes
  S $LIB_PREFIX/lib/findlib
  S $LIB_PREFIX/lib/ocaml
  # Processing lib/.merlin
  B ../_build/default/lib/.bar.objs
  B ../_build/default/lib/.foo.objs
  B $LIB_PREFIX/lib/bytes
  B $LIB_PREFIX/lib/findlib
  B $LIB_PREFIX/lib/ocaml
  FLG -open Foo -w -40 -open Bar -w -40
  S .
  S $LIB_PREFIX/lib/bytes
  S $LIB_PREFIX/lib/findlib
  S $LIB_PREFIX/lib/ocaml

Make sure a ppx directive is generated

  $ grep -q ppx lib/.merlin
  [1]

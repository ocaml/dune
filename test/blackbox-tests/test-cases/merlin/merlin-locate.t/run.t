Check the "A" source code reference in [A.nothing] in b.ml can be located

  $ LIBNAME=basic
  $ dune clean
  $ dune build ./$LIBNAME/.merlin-conf/lib-$LIBNAME $LIBNAME/$LIBNAME.cma --profile release
  $ find _build -name *.ml | sort
  _build/default/basic/a.ml
  _build/default/basic/b.ml
  $ dune ocaml dump-dot-merlin _build/default/$LIBNAME/ | tee .merlin | grep -e "^B " -e "^S "
  B $TESTCASE_ROOT/_build/default/basic/.basic.objs/byte
  S $TESTCASE_ROOT/basic
  S $TESTCASE_ROOT/_build/default/basic
  $ cat ./$LIBNAME/b.ml | ocamlmerlin single locate -look-for ml -position 1:10 -filename b.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/_build/default/basic/a.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

Check the "A" copied (copy#) source code reference [A.nothing] can be located and
that Merlin resolves it back to the original "A".

If the roundtrip fails, we'd get:
> "'Copy_of_a' seems to originate from 'Copy_of_a' whose ML file could not be found"

  $ LIBNAME=copyroundtrip
  $ dune clean
  $ dune build ./$LIBNAME/.merlin-conf/lib-$LIBNAME $LIBNAME/$LIBNAME.cma --profile release
  $ find _build -name *.ml | sort
  _build/default/copyroundtrip/a.ml
  _build/default/copyroundtrip/b.ml
  _build/default/copyroundtrip/copy_of_a.ml
  $ dune ocaml dump-dot-merlin _build/default/$LIBNAME/ | tee .merlin | grep -e "^B " -e "^S "
  B $TESTCASE_ROOT/_build/default/copyroundtrip/.copyroundtrip.objs/byte
  S $TESTCASE_ROOT/copyroundtrip
  S $TESTCASE_ROOT/_build/default/copyroundtrip
  $ cat _build/default/copyroundtrip/copy_of_a.ml
  # 1 "copyroundtrip/a.ml"
  let value = 3
  $ cat ./$LIBNAME/b.ml | ocamlmerlin single locate -look-for ml -position 1:10 -filename b.ml | jq .value
  {
    "file": "$TESTCASE_ROOT/_build/default/copyroundtrip/copy_of_a.ml",
    "pos": {
      "line": 1,
      "col": 0
    }
  }
  $ echo "PENDING Upstream issue: ocamlmerlin should read line header and report 'copyroundtrip/a.ml' not the copied file."
  PENDING Upstream issue: ocamlmerlin should read line header and report 'copyroundtrip/a.ml' not the copied file.

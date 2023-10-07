  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ ocamlfind_libs="$(ocamlfind printconf path | while read line; do printf lib=${line}:; done)"
  $ export BUILD_PATH_PREFIX_MAP="$ocamlfind_libs:$BUILD_PATH_PREFIX_MAP"

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name pp_future_syntax)
  >  (preprocess future_syntax))
  > EOF

  $ touch pp_future_syntax.ml

  $ dune build ./.merlin-conf/exe-pp_future_syntax --profile release
  $ dune ocaml merlin dump-config .
  Pp_future_syntax
  ((STDLIB /OCAMLC_WHERE)
   (EXCLUDE_QUERY_DIR)
   (B
    $TESTCASE_ROOT/_build/default/.pp_future_syntax.eobjs/byte)
   (S
    $TESTCASE_ROOT)
   (FLG (-w -40 -g)))

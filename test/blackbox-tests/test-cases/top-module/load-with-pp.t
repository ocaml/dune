  $ make_dune_project 3.3

  $ cat >dune <<EOF
  > (library
  >  (preprocess (action (run %{bin:dunepp} %{input-file})))
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > let foo = _STRING_
  > EOF

  $ dune ocaml top-module foo.ml | sed 's/"[^"]*dunepp"/$dunepp/g'
  #directory "$TESTCASE_ROOT/_build/default/.topmod/foo.ml";;
  #load "$TESTCASE_ROOT/_build/default/.topmod/foo.ml/foo.cmo";;
  #pp $dunepp;;

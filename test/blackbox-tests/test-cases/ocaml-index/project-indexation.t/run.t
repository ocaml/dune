  $ mkdir bin
  $ ln -s $(which ocaml_index) bin/ocaml-index
  $ export PATH=bin:$PATH

  $ dune exec ./main.exe
  1564242

The @check alias does not build indexes:
(it might at some point if the process becomes fast-enough)

  $ dune build @check

  $ find . -name '*.ocaml-index' | sort

The @ocaml-index indexes the entire workspace, including librairies that might
not be directly used and thus usually not built by @check:

  $ dune build @ocaml-index

  $ find . -name '*.ocaml-index' | sort
  ./_build/default/.main.eobjs/cctx.ocaml-index
  ./_build/default/implicit-lib/.imp_lib.objs/cctx.ocaml-index
  ./_build/default/lib/.otherlib.objs/cctx.ocaml-index
  ./_build/default/private-module/.pmodlib.objs/cctx.ocaml-index
  ./_build/default/sub-project/.subprojectlib.objs/cctx.ocaml-index
  ./_build/default/vendor/otherproject/.private_lib.objs/cctx.ocaml-index
  ./_build/default/vendor/otherproject/.vendored_lib.objs/cctx.ocaml-index


  $ dune ocaml merlin dump-config --format=json . |
  > jq -r '.[0].config[] | select(.[0] == "INDEX") | "INDEX \(.[1])"'
  INDEX $TESTCASE_ROOT/_build/default/.main.eobjs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/implicit-lib/.imp_lib.objs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/lib/.otherlib.objs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/private-module/.pmodlib.objs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/sub-project/.subprojectlib.objs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/vendor/otherproject/.private_lib.objs/cctx.ocaml-index
  INDEX $TESTCASE_ROOT/_build/default/vendor/otherproject/.vendored_lib.objs/cctx.ocaml-index

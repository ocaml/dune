  $ mkdir bin
  $ cp $(which ocaml_index) bin/ocaml-index
  $ export PATH=bin:$PATH

Building from the workspace folder creates all three indexes:
  $ dune build @ocaml-index
  $ find . -name '*.ocaml-index' | sort
  ./_build/default/sub-project/bin/.main.eobjs/cctx.ocaml-index
  ./_build/default/sub-project/lib/.subprojectlib.objs/cctx.ocaml-index
  ./_build/default/sub-project2/lib/.subprojectlib2.objs/cctx.ocaml-index

  $ dune clean

Building from one of the sub-projects folder also creates all three indexes:
  $ cd sub-project 
  $ export PATH=../bin:$PATH
  $ dune build --workspace=../dune-workspace --root=.. @sub-project/ocaml-index
  Entering directory '..'
  Leaving directory '..'
  $ cd ..

  $ find . -name '*.ocaml-index' | sort
  ./_build/default/sub-project/bin/.main.eobjs/cctx.ocaml-index
  ./_build/default/sub-project/lib/.subprojectlib.objs/cctx.ocaml-index
  ./_build/default/sub-project2/lib/.subprojectlib2.objs/cctx.ocaml-index

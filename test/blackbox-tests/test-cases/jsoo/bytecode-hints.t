Starting with OCaml 5.6, the bytecode executables compiled by js_of_ocaml and
wasm_of_ocaml in whole program mode are linked with -bytecode-hints, so that
the optimization hints of the compilation units are preserved.

  $ make_dune_project 3.7

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (modes js))
  > EOF
  $ touch main.ml

  $ dune build --profile release main.bc-for-jsoo
  $ dune trace cat | jq_dune -c 'processes | select(.args | targets | any(endswith("main.bc-for-jsoo"))) | .args.process_args'
  ["-w","-40","-g","-o","main.bc-for-jsoo","-no-check-prims","-noautolink","-bytecode-hints",".main.eobjs/byte/dune__exe__Main.cmo"]

A root module for a stanza using a PPX should contain modules from the direct
libraries only. The PPX runtime libraries are implementation details of the PPX
and should not be re-exported by the generated root module.

  $ make_dune_project 3.23
  $ mkdir ppx
  $ cat >ppx/dune <<EOF
  > (library
  >  (name ppx_runtime)
  >  (modules ppx_runtime))
  > (library
  >  (name ppx)
  >  (modules ppx)
  >  (kind ppx_rewriter)
  >  (ppx_runtime_libraries ppx_runtime)
  >  (ppx.driver (main Ppx.main)))
  > EOF
  $ cat >ppx/ppx_runtime.ml <<EOF
  > let message = "runtime"
  > EOF
  $ cat >ppx/ppx.mli <<EOF
  > val main : unit -> unit
  > EOF
  $ cat >ppx/ppx.ml <<EOF
  > let main () =
  >   let out = ref "" in
  >   let args =
  >     [ "-o", Arg.Set_string out, ""
  >     ; "--impl", Arg.Set_string (ref ""), ""
  >     ; "--as-ppx", Arg.Set (ref false), ""
  >     ]
  >   in
  >   let anon _ = () in
  >   Arg.parse (Arg.align args) anon "";
  >   let out = open_out !out in
  >   close_out out
  > EOF
  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (root_module root)
  >  (libraries unix)
  >  (preprocess (pps ppx)))
  > EOF
  $ cat >main.ml <<EOF
  > module X = Root.Unix
  > EOF
  $ dune build
  $ cat _build/default/root.ml-gen
  module Ppx_runtime = Ppx_runtime
  module Unix = Unix
  module UnixLabels = UnixLabels

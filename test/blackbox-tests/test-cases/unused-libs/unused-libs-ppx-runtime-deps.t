Demonstrate ppx_runtime_deps issues

  $ make_dune_project 3.21

  $ cat >dune <<EOF
  > (library
  >  (modules ())
  >  (name runtime))
  > (library
  >  (name foo)
  >  (modules ())
  >  (kind ppx_rewriter)
  >  (ppx_runtime_libraries runtime)
  >  (libraries compiler-libs.common)
  >  (ppx.driver (main "\| (fun () ->
  >                    "\|    output_string (open_out "use/use.pp.ml") "let () = ()")
  >                    )))
  > EOF

  $ mkdir use
  $ cat >use/dune <<EOF
  > (executable
  >  (name use)
  >  (preprocess (pps foo)))
  > EOF
  $ cat >use/use.ml <<EOF
  > let () = ()
  > EOF

  $ dune build @use/unused-libs

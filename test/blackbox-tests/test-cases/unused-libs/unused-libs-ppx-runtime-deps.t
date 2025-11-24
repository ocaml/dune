Demonstrate ppx_runtime_deps issues

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

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
  File "use/dune", lines 1-3, characters 0-48:
  1 | (executable
  2 |  (name use)
  3 |  (preprocess (pps foo)))
  Error: Unused libraries:
  - runtime
  [1]

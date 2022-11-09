Load a module that requires ppx

  $ cat >dune-project <<EOF
  > (lang dune 3.3)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name driver_print_args)
  >  (kind ppx_rewriter)
  >  (modules ())
  >  (ppx.driver (main "\| (fun () ->
  >                    "\|   let out = Sys.argv.(4) in
  >                    "\|   let out = open_out out in
  >                    "\|   let rec loop () =
  >                    "\|     match input_line stdin with
  >                    "\|     | s -> output_string out (s ^ "\n"); loop ()
  >                    "\|     | exception End_of_file -> close_out out
  >                    "\|   in loop ())
  >                    )))
  > (library
  >  (name foo)
  >  (preprocess (pps driver_print_args)))
  > EOF
  $ cat >foo.ml <<EOF
  > let () = ()
  > EOF
  $ dune ocaml top-module foo.ml
  #directory "$TESTCASE_ROOT/_build/default/.topmod/foo.ml";;
  #load "$TESTCASE_ROOT/_build/default/.topmod/foo.ml/foo.cmo";;
  #ppx "$TESTCASE_ROOT/_build/default/.ppx/d464a1bb671660981248d354c1722d5f/ppx.exe --as-ppx --cookie 'library-name=\"foo\"'";;
  $ basename $(ls _build/default/.ppx/*/*.exe)
  ppx.exe

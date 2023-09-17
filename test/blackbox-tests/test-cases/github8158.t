Repro for #8158: we set up a ppx rewriter (that does nothing) in ppx/ and an
executable that uses as staged_pps it in bin/.
This test ensures that path expansion is done relatively to the right
directory.

  $ cat > dune-project << EOF
  > (lang dune 1.1)
  > EOF

  $ mkdir ppx
  $ cat > ppx/dune << EOF
  > (library
  >  (name ppx_noop)
  >  (kind ppx_rewriter)
  >  (ppx.driver (main Ppx_noop.main)))
  > EOF
  $ cat > ppx/ppx_noop.ml << EOF
  > let main () =
  >   match Sys.argv with
  >   | [| _; "--as-ppx"; input_file; output_file |] ->
  >      Filename.quote_command "cp" [input_file; output_file]
  >      |> Sys.command
  >      |> exit
  >   | _ -> assert false
  > EOF

  $ mkdir bin
  $ cat > bin/dune << EOF
  > (executable
  >  (name e)
  >  (preprocess
  >   (staged_pps ppx_noop)))
  > EOF
  $ touch bin/e.ml

This works:

  $ dune exec bin/e.exe

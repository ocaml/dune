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

This fails, but should work (and print nothing):

  $ dune exec bin/e.exe 2>&1 | sed -e 's/camlppx....../camlppxXXXXXX/g' -e 's/build_......_dune/build_XXXXXX_dune/g'
  File "bin/.e.eobjs/_unknown_", line 1, characters 0-0:
  sh: 1: ../.ppx/059965dd2a1761d0e00c111505aef5ac/ppx.exe: not found
  File "bin/e.ml", line 1:
  Error: I/O error: ../.ppx/059965dd2a1761d0e00c111505aef5ac/ppx.exe --as-ppx '/tmp/build_XXXXXX_dune/build_XXXXXX_dune/camlppxXXXXXX' '/tmp/build_XXXXXX_dune/build_XXXXXX_dune/camlppxXXXXXX'

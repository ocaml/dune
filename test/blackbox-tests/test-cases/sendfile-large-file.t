We create a large file and check that it is copied completely.

  $ make_dune_project 1.0

  $ cat > create.ml << EOF
  > let () = Unix.truncate "file.dat" 0x1_00_00_00_03
  > EOF
  $ touch file.dat
  $ ocaml -I +unix unix.cma create.ml
  $ rm create.ml

  $ dune build file.dat

  $ dune_cmd stat size file.dat
  4294967299

  $ dune_cmd stat size _build/default/file.dat
  4294967299

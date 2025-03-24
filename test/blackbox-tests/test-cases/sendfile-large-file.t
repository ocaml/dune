We create a large file and check that it is copied completely.

  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > EOF

  $ cat > create.ml << EOF
  > let () = Unix.truncate "file.dat" 0x1_00_00_00_03
  > EOF
  $ touch file.dat
  $ ocaml unix.cma create.ml
  $ rm create.ml

  $ dune build file.dat

  $ dune_cmd stat size file.dat
  4294967299

  $ dune_cmd stat size _build/default/file.dat
  4294967299


  $ dune build
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.

  $ ls _build/default
  Datatypes.ml
  Datatypes.mli
  extraction
  my_coq_file.ml
  my_coq_file.mli
  my_prog.exe
  my_prog.ml
  my_prog.mli

  $ cat _build/default/Datatypes.ml
  
  type bool =
  | Coq_true
  | Coq_false

  $ dune exec -- ./my_prog.exe
  Warning: Coq Language Versions lower than 0.8 have been deprecated in Dune
  3.8 and will be removed in an upcoming Dune version.
  Result: false

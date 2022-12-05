
  $ dune build

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
  Result: false

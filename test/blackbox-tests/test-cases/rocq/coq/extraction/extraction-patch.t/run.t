
  $ dune build
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))

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
  Warning: Dune's Coq Build Language is deprecated, and will be removed in Dune
  3.24. Please upgrade to the new Rocq Build Language.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (deprecated_coq_lang disabled))
  Result: false


  $ dune build
  File "./my_coq_file.v", line 8, characters 0-23:
  Warning: Setting extraction output directory by default to
  "$TESTCASE_ROOT/_build/default/extraction".
  Use "Set Extraction Output Directory" or command line option
  "-output-directory" to set a different directory for extracted files to
  appear in. [extraction-default-directory,extraction,default]

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

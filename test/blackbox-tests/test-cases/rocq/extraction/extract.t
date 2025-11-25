  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ cat >extract.v <<EOF
  > Definition nb (b : bool) : bool :=
  >   match b with
  >   | false => true
  >   | true => false
  >   end.
  > 
  > Require Extraction.
  > Separate Extraction nb.
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extract)
  >  (extracted_modules Datatypes extract))
  > 
  > (executable
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > open Datatypes
  > let () =
  >   print_endline (
  >     match Extract.nb Datatypes.Coq_true with
  >     | Coq_true -> "true"
  >     | Coq_false -> "false"
  >   )
  > EOF

  $ dune exec ./foo.exe
  File "./extract.v", line 8, characters 0-23:
  Warning: Setting extraction output directory by default to
  "$TESTCASE_ROOT/_build/default".
  Use "Set Extraction Output Directory" or command line option
  "-output-directory" to set a different directory for extracted files to
  appear in. [extraction-default-directory,extraction,default]
  false
  $ ls _build/default
  Datatypes.ml
  Datatypes.mli
  extract.glob
  extract.ml
  extract.mli
  extract.v
  extract.vo
  extract.vok
  extract.vos
  foo.exe
  foo.ml
  foo.mli

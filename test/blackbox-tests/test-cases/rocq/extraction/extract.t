  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.12)
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
  foo.exe
  foo.ml
  foo.mli

  $ cat >extract.expected <<EOF
  > Hello!
  > EOF
  $ dune runtest
  File "./extract.v", line 8, characters 0-23:
  Warning: Setting extraction output directory by default to
  "$TESTCASE_ROOT/_build/default".
  Use "Set Extraction Output Directory" or command line option
  "-output-directory" to set a different directory for extracted files to
  appear in. [extraction-default-directory,extraction,default]
  File "extract.expected", line 1, characters 0-0:
  --- extract.expected
  +++ extract.output
  @@ -1 +0,0 @@
  -Hello!
  [1]
  $ dune promote
  Promoting _build/default/extract.output to extract.expected.
  $ dune runtest

Make sure that the error message is helpful if a module is both covered by a theory and an extraction stanza.
  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.12)
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
  > (rocq.theory
  >  (name TestExtr))
  > (rocq.extraction
  >  (prelude extract)
  >  (extracted_modules Datatypes extract))
  > EOF
  $ dune build
  File "dune", lines 3-5, characters 0-75:
  3 | (rocq.extraction
  4 |  (prelude extract)
  5 |  (extracted_modules Datatypes extract))
  Error: Duplicate Rocq module "extract".
  Hint: The Rocq module "extract" is already covered by rocq.theory stanza
  "TestExtr".
  [1]

Make sure that the error message is helpful if a module is both covered by an extraction and a theory stanza.
  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.12)
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
  > (rocq.theory
  >  (name TestExtr))
  > EOF
  $ dune build
  File "dune", lines 4-5, characters 0-30:
  4 | (rocq.theory
  5 |  (name TestExtr))
  Error: Duplicate Rocq module "extract".
  Hint: The Rocq module "extract" is already covered by rocq.extraction stanza
  "extract".
  [1]

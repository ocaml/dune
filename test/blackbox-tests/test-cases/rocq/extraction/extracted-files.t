Test error when extracted_files is not provided in rocq 0.13:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >extr.v <<EOF
  > Definition nb := true.
  > Require Extraction.
  > Extraction Language OCaml.
  > Separate Extraction nb.
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr))
  > EOF

  $ dune build
  File "dune", lines 1-2, characters 0-33:
  1 | (rocq.extraction
  2 |  (prelude extr))
  Error: Field "extracted_files" is required
  [1]

Test version gating: extracted_files requires (rocq 0.13):

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.12)
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_files foo.hs))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-25:
  3 |  (extracted_files foo.hs))
       ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'extracted_files' is only available since version 0.13 of Rocq Prover
  build language. Please update your dune-project file to have (using rocq
  0.13).
  [1]

Test that extracted_modules is deleted in rocq 0.13 with a helpful error message:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_modules extr)
  >  (flags (:standard -w -extraction-default-directory)))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-25:
  3 |  (extracted_modules extr)
       ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'extracted_modules' was deleted in version 0.13 of Rocq Prover build
  language. Use (extracted_files ...) instead, listing each .ml and .mli file
  explicitly.
  [1]

Test using extracted_files with explicit filenames:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >extr.v <<EOF
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
  >  (prelude extr)
  >  (extracted_files Datatypes.ml Datatypes.mli extr.ml extr.mli)
  >  (flags (:standard -w -extraction-default-directory)))
  > EOF

  $ dune build
  $ ls _build/default/Datatypes.ml _build/default/extr.ml
  _build/default/Datatypes.ml
  _build/default/extr.ml

Test that extracted_modules in 0.13 gives deleted_in error for Haskell extraction:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >extr.v <<EOF
  > Definition nb (b : bool) : bool :=
  >   match b with
  >   | false => true
  >   | true => false
  >   end.
  > 
  > Require Extraction.
  > Extraction Language Haskell.
  > Separate Extraction nb.
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_modules Datatypes.hs extr.hs)
  >  (flags (:standard -w -extraction-default-directory)))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-41:
  3 |  (extracted_modules Datatypes.hs extr.hs)
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'extracted_modules' was deleted in version 0.13 of Rocq Prover build
  language. Use (extracted_files ...) instead, listing each .ml and .mli file
  explicitly.
  [1]

Test using extracted_files with Haskell outputs (expected success):

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >extr.v <<EOF
  > Definition nb (b : bool) : bool :=
  >   match b with
  >   | false => true
  >   | true => false
  >   end.
  > 
  > Require Extraction.
  > Extraction Language Haskell.
  > Separate Extraction nb.
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_files Datatypes.hs extr.hs)
  >  (flags (:standard -w -extraction-default-directory)))
  > EOF

  $ dune build
  $ ls _build/default | sort
  Datatypes.hs
  extr.glob
  extr.hs
  extr.v
  extr.vo

Test rebuild does not clean extracted files:
(NOTE: re-using above pre-built context)
  $ dune build
  $ ls _build/default | sort
  Datatypes.hs
  extr.glob
  extr.hs
  extr.v
  extr.vo

Test that extracted_modules in 0.13 gives deleted_in error for Scheme extraction:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >extr.v <<EOF
  > Definition nb (b : bool) : bool :=
  >   match b with
  >   | false => true
  >   | true => false
  >   end.
  > 
  > Require Extraction.
  > Extraction Language Scheme.
  > Extraction "nb.scm" nb.
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_modules nb.scm)
  >  (flags (:standard -w -extraction-default-directory)))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-27:
  3 |  (extracted_modules nb.scm)
       ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'extracted_modules' was deleted in version 0.13 of Rocq Prover build
  language. Use (extracted_files ...) instead, listing each .ml and .mli file
  explicitly.
  [1]

Test using extracted_files with Scheme outputs (expected success):

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >extr.v <<EOF
  > Definition nb (b : bool) : bool :=
  >   match b with
  >   | false => true
  >   | true => false
  >   end.
  > 
  > Require Extraction.
  > Extraction Language Scheme.
  > Extraction "nb.scm" nb.
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_files nb.scm)
  >  (flags (:standard -w -extraction-default-directory)))
  > EOF

  $ dune build
  $ ls _build/default | sort
  extr.glob
  extr.v
  extr.vo
  nb.scm

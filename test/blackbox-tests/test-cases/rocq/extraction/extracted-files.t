Test error when neither extracted_modules nor extracted_files is provided:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >extr.v <<EOF
  > Definition nb := true.
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr))
  > EOF

  $ dune build
  File "dune", lines 1-2, characters 0-33:
  1 | (rocq.extraction
  2 |  (prelude extr))
  Error: At least one of (extracted_modules) or (extracted_files) must be
  specified
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

Test duplicate target filename across extracted_modules and extracted_files:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_modules foo)
  >  (extracted_files foo.ml))
  > EOF

  $ dune build
  File "dune", lines 1-4, characters 0-84:
  1 | (rocq.extraction
  2 |  (prelude extr)
  3 |  (extracted_modules foo)
  4 |  (extracted_files foo.ml))
  Error: Duplicate target filename "foo.ml" across extracted_modules and
  extracted_files
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

Test using extracted_modules with Haskell outputs (expected failure):

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
  File "dune", lines 1-4, characters 0-129:
  1 | (rocq.extraction
  2 |  (prelude extr)
  3 |  (extracted_modules Datatypes.hs extr.hs)
  4 |  (flags (:standard -w -extraction-default-directory)))
  Error: Rule failed to generate the following targets:
  - Datatypes.hs.ml
  - Datatypes.hs.mli
  - extr.hs.ml
  - extr.hs.mli
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
  extr.vok
  extr.vos

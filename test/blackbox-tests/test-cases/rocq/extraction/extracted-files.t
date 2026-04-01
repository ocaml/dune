Test error when neither extracted_modules nor extracted_files is provided:

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

Test warning on duplicate target filename across extracted_modules and extracted_files:

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_modules extr)
  >  (extracted_files extr.ml)
  >  (flags (:standard -w -extraction-default-directory)))
  > EOF

  $ dune build
  File "dune", lines 1-5, characters 0-140:
  1 | (rocq.extraction
  2 |  (prelude extr)
  3 |  (extracted_modules extr)
  4 |  (extracted_files extr.ml)
  5 |  (flags (:standard -w -extraction-default-directory)))
  Warning: Duplicate target filename "extr.ml" across (extracted_modules) and
  (extracted_files)
  $ ls _build/default | sort
  Datatypes.ml
  Datatypes.mli
  extr.glob
  extr.ml
  extr.mli
  extr.v
  extr.vo
  extr.vok
  extr.vos

Test completeness of both extracted_modules and extracted_files (expected failure):

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (using rocq 0.13)
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_modules extr)
  >  (extracted_files fake.hs)
  >  (flags (:standard -w -extraction-default-directory)))
  > EOF

  $ dune build
  File "dune", lines 1-5, characters 0-140:
  1 | (rocq.extraction
  2 |  (prelude extr)
  3 |  (extracted_modules extr)
  4 |  (extracted_files fake.hs)
  5 |  (flags (:standard -w -extraction-default-directory)))
  Error: Rule failed to generate the following targets:
  - fake.hs
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

Test rebuild does not clean extracted files:
(NOTE: re-using above pre-built context)
  $ dune build
  $ ls _build/default | sort
  Datatypes.hs
  extr.glob
  extr.hs
  extr.v
  extr.vo

Test using extracted_modules with Scheme outputs (expected failure):

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
  File "dune", lines 1-4, characters 0-115:
  1 | (rocq.extraction
  2 |  (prelude extr)
  3 |  (extracted_modules nb.scm)
  4 |  (flags (:standard -w -extraction-default-directory)))
  Error: Rule failed to generate the following targets:
  - nb.scm.ml
  - nb.scm.mli
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
  extr.vok
  extr.vos
  nb.scm

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ cat >Extr1.v <<EOF
  > Require Import Corelib.extraction.Extraction.
  > Extraction "Mod1.ml" app.
  > EOF

  $ cat >Extr2.v <<EOF
  > Require Import Corelib.extraction.Extraction.
  > Extraction "Mod2.ml" length.
  > EOF

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude Extr1)
  >  (extracted_modules Mod1)
  >  (flags (:standard -w -extraction-default-directory)))
  > 
  > (rocq.extraction
  >  (prelude Extr2)
  >  (extracted_modules Mod2)
  >  (flags (:standard -w -extraction-default-directory)))
  > EOF

  $ dune build
  $ ls _build/default | sort
  Extr1.glob
  Extr1.v
  Extr1.vo
  Extr1.vok
  Extr1.vos
  Extr2.glob
  Extr2.v
  Extr2.vo
  Extr2.vok
  Extr2.vos
  Mod1.ml
  Mod1.mli
  Mod2.ml
  Mod2.mli
  $ ls _build/default/.*.d | sort
  _build/default/.Dune_Extraction_Extr1.theory.d
  _build/default/.Dune_Extraction_Extr2.theory.d

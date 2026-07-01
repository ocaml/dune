Tests Menhir stanzas under env settings.

  $ make_menhir_project 2.2 2.1
  $ cat >dune <<EOF
  > (env (_ (menhir_flags :standard "--comment")))
  > (menhir
  >  (modules parser)
  >  (mode promote))
  > (library (name test))
  > EOF
  $ dune printenv --field menhir_flags 2>&1 | sed "s/(using menhir .*)/(using menhir <version>)/"
  (menhir_flags (--comment))

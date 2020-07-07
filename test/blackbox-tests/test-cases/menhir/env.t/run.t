  $ echo "(lang dune 2.2)" > dune-project
  $ cat >dune <<EOF
  > (env (_ (menhir_flags :standard "--comment")))
  > (menhir
  >  (modules parser)
  >  (mode promote))
  > (library (name test))
  > EOF
  $ dune printenv --field menhir_flags 2>&1 | sed "s/(using menhir .*)/(using menhir <version>)/"
  Info: Appending this line to dune-project: (using menhir <version>)
  (menhir_flags (--comment))

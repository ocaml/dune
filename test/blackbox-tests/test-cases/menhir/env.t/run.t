  $ cat > dune-project <<EOF
  > (lang dune 2.2)
  > (using menhir 2.1)
  > EOF
  $ cat >dune <<EOF
  > (env (_ (menhir_flags :standard "--comment")))
  > (menhir
  >  (modules parser)
  >  (mode promote))
  > (library (name test))
  > EOF
  $ dune printenv --field menhir_flags 2>&1 | sed "s/(using menhir .*)/(using menhir <version>)/"
  (menhir_flags (--comment))

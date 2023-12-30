Testing the Extra Dependency command in coqc and how dune is able to support its
dependencies.

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using coq 0.9)
  > EOF

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (extra_sources a))
  > EOF

  $ cat > a
  $ cat > b.v <<EOF
  > From foo Extra Dependency "a".
  > EOF

  $ dune build b.vo

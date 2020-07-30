In github #3624, dune created a dune-project with an incorrect using line.

  $ cat >dune <<EOF
  > (coq.theory
  >  (name foo))
  > EOF
  $ dune build 2>&1 | grep using
  Info: Appending this line to dune-project: (using coq 0.2)

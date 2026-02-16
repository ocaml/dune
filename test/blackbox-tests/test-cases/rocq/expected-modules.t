Test that expected files work with explicit (modules ...).
Currently expected files are ignored when using explicit (modules ...):

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name a)
  >  (modules foo))
  > EOF

  $ cat > foo.v <<EOF
  > Locate nat.
  > EOF

  $ touch foo.expected
  $ dune runtest

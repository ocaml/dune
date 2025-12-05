Test that the warning for coq lang 0.8 can be disabled

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > EOF

  $ cat > dune <<EOF
  > (rocq.theory
  >  (name Foo))
  > EOF

  $ cat > a.v <<EOF
  > Definition a := Type.
  > EOF

  $ dune build

We get the warning again, even on a zero-build:

  $ dune build

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > (using rocq 0.11)
  > (warnings (deprecated_coq_lang_lt_08 disabled))
  > EOF

  $ dune build

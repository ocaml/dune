Test error message when user adds extra parenthesis around dependencies: (#12784)

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name bar)
  >  (libraries (abc123 foo)))
  > EOF

  $ dune build
  File "dune", line 3, characters 12-24:
  3 |  (libraries (abc123 foo)))
                  ^^^^^^^^^^^^
  Error: Atom or quoted string expected
  [1]

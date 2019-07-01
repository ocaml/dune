
  $ cat > dune-project <<EOF
  > (lang dune 1.11)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >   (targets a b)
  >   (action (with-stdout-to %{targets} (echo "hola"))))
  > 
  > EOF

  $ dune build a
  File "dune", line 3, characters 28-36:
  3 |   (action (with-stdout-to %{targets} (echo "hola"))))
                                  ^^^^^^^^
  Error: Variable %{targets} expands to 2 values, however a single value is
  expected here. Please quote this atom.
  [1]

# CR-someday aalekseyev: the suggestion above is nonsense!
# quoting the atom will achieve nothing.

  $ cat > dune <<EOF
  > (rule
  >   (targets a b)
  >   (action (bash "echo hola > %{targets}")))
  > EOF

  $ dune build a
  File "dune", line 1, characters 0-65:
  1 | (rule
  2 |   (targets a b)
  3 |   (action (bash "echo hola > %{targets}")))
  Error: Rule failed to generate the following targets:
  - b
  [1]

^ the echo command may succeed, but what it does is total nonsense
Therefore the user is encouraged to write singular [target] where it makes sense
to get a better error message:

  $ cat > dune <<EOF
  > (rule
  >   (targets a b)
  >   (action (bash "echo hola > %{target}")))
  > EOF

  $ dune build a
  File "dune", line 3, characters 31-38:
  3 |   (action (bash "echo hola > %{target}")))
                                     ^^^^^^^
  Error: There is more than one target. %{target} requires there to be one
  unambiguous target.
  [1]

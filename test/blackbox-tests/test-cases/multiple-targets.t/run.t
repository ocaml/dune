
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
  Error: You can only use the variable %{target} if you defined the list of
  targets using the field [target] (not [targets])
  [1]

^ Expected error message

  $ cat > dune <<EOF
  > (rule
  >   (targets a)
  >   (target a)
  >   (action (bash "echo hola > %{target}")))
  > EOF

  $ dune build a
  File "dune", line 1, characters 0-75:
  1 | (rule
  2 |   (targets a)
  3 |   (target a)
  4 |   (action (bash "echo hola > %{target}")))
  Error: fields targets, target are mutually exclusive
  [1]

^ Specifying both [targets] and [target] is not allowed

  $ cat > dune <<EOF
  > (rule
  >   (targets a)
  >   (action (bash "echo hola > %{target}")))
  > EOF

  $ dune build a
  File "dune", line 3, characters 31-38:
  3 |   (action (bash "echo hola > %{target}")))
                                     ^^^^^^^
  Error: You can only use the variable %{target} if you defined the list of
  targets using the field [target] (not [targets])
  [1]

^ Expected error

  $ cat > dune <<EOF
  > (rule
  >   (target a)
  >   (action (bash "echo hola > %{targets}")))
  > EOF

  $ dune build a
  File "dune", line 3, characters 31-39:
  3 |   (action (bash "echo hola > %{targets}")))
                                     ^^^^^^^^
  Error: You can only use the variable %{targets} if you defined the list of
  targets using the field [targets] (not [target])
  [1]

^ Expected error

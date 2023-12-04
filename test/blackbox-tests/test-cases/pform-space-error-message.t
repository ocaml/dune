  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (action
  >    (run %{bi{n:echo})))
  > EOF

  $ dune build out.txt
  File "dune", line 3, characters 12-13:
  3 |    (run %{bi{n:echo})))
                  ^
  Error: The character '{' is not allowed inside %{...} forms
  [1]

  $ cat >dune <<EOF
  > (rule
  >  (action
  >    (run %{bin:ec(ho})))
  > EOF

  $ dune build out.txt
  File "dune", line 3, characters 16-17:
  3 |    (run %{bin:ec(ho})))
                      ^
  Error: The character '(' is not allowed inside %{...} forms
  [1]

  $ dune build out.txt
  File "dune", line 3, characters 16-17:
  3 |    (run %{bin:ec(ho})))
                      ^
  Error: The character '(' is not allowed inside %{...} forms
  [1]
  $ cat >dune <<EOF
  > (rule
  >  (action
  >    (run %{bin : echo})))
  > EOF

  $ dune build out.txt
  File "dune", line 3, characters 13-14:
  3 |    (run %{bin : echo})))
                   ^
  Error: The character ' ' is not allowed inside %{...} forms
  [1]


Documents what dune accepts as a (deps (package ...)) name. The only
validation at the boundary is "non-empty"; anything else passes through
and ultimately fails as "Package X does not exist". See the strict
decoder-level validation in package-name-strict.t for the dune-project
(package (name ...)) side.

  $ make_dune_project 3.24

  $ test() {
  > cat > dune << EOF
  > (rule
  >  (deps (package $1))
  >  (target out)
  >  (action (with-stdout-to out (echo "ok"))))
  > EOF
  > dune build out
  > }

Literal empty name:

  $ test '""'
  File "dune", line 2, characters 16-18:
  2 |  (deps (package ""))
                      ^^
  Error: "" is an invalid package name.
  [1]

Expansion that resolves to the empty string:

  $ unset NOPE; test '%{env:NOPE=}'
  File "dune", line 2, characters 16-28:
  2 |  (deps (package %{env:NOPE=}))
                      ^^^^^^^^^^^^
  Error: "" is an invalid package name.
  [1]

Whitespace-only name passes the name check and reaches the lookup:

  $ test '" "'
  File "dune", line 2, characters 16-19:
  2 |  (deps (package " "))
                      ^^^
  Error: Package   does not exist
  [1]

Characters opam would reject (e.g. '&') are accepted by dune:

  $ test '"foo&bar"'
  File "dune", line 2, characters 16-25:
  2 |  (deps (package "foo&bar"))
                      ^^^^^^^^^
  Error: Package foo&bar does not exist
  [1]

A dot is also accepted, even though opam would reject it:

  $ test '"foo.bar"'
  File "dune", line 2, characters 16-25:
  2 |  (deps (package "foo.bar"))
                      ^^^^^^^^^
  Error: Package foo.bar does not exist
  [1]

All-numeric (no letter) is accepted, even though opam would reject it:

  $ test 123
  File "dune", line 2, characters 16-19:
  2 |  (deps (package 123))
                      ^^^
  Error: Package 123 does not exist
  [1]

Uppercase letters are accepted:

  $ test FOO
  File "dune", line 2, characters 16-19:
  2 |  (deps (package FOO))
                      ^^^
  Error: Package FOO does not exist
  [1]

Leading dash is accepted:

  $ test '"-foo"'
  File "dune", line 2, characters 16-22:
  2 |  (deps (package "-foo"))
                      ^^^^^^
  Error: Package -foo does not exist
  [1]

A normal-looking but non-existent name reaches the same lookup error:

  $ test nonexistent
  File "dune", line 2, characters 16-27:
  2 |  (deps (package nonexistent))
                      ^^^^^^^^^^^
  Error: Package nonexistent does not exist
  [1]

Whitespace inside an expansion fails at expansion time, not at the name
check:

  $ test '%{env:X=foo bar}'
  File "dune", line 2, characters 27-28:
  2 |  (deps (package %{env:X=foo bar}))
                                 ^
  Error: The character ' ' is not allowed inside %{...} forms
  [1]

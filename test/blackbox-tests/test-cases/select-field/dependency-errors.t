Dependency validation distinguishes required, optional (in a select), and
forbidden dependencies before resolving any libraries.

  $ make_dune_project 3.22
  $ cat > dune <<'EOF'
  > (library (name foo) (modules) (libraries missing missing))
  > EOF
  $ dune build
  File "dune", line 1, characters 30-57:
  1 | (library (name foo) (modules) (libraries missing missing))
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: library "missing" is present twice
  [1]

A name cannot be both required and forbidden in one clause.

  $ cat > dune <<'EOF'
  > (library (name foo) (modules)
  >  (libraries (select choice.ml from (missing !missing -> choice.none.ml))))
  > EOF
  $ dune build
  File "dune", line 2, characters 35-71:
  2 |  (libraries (select choice.ml from (missing !missing -> choice.none.ml))))
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: library "missing" is both required and forbidden in this clause
  [1]

A required dependency cannot also be optional or forbidden in another clause.
Check both orders so neither form can overwrite the previous one silently.

  $ cat > dune <<'EOF'
  > (library (name foo) (modules)
  >  (libraries missing (select choice.ml from (missing -> choice.some.ml))))
  > EOF
  $ dune build
  File "dune", line 2, characters 1-72:
  2 |  (libraries missing (select choice.ml from (missing -> choice.some.ml))))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: library "missing" is present both as an optional and required
  dependency
  [1]
  $ cat > dune <<'EOF'
  > (library (name foo) (modules)
  >  (libraries (select choice.ml from (missing -> choice.some.ml)) missing))
  > EOF
  $ dune build
  File "dune", line 2, characters 1-72:
  2 |  (libraries (select choice.ml from (missing -> choice.some.ml)) missing))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: library "missing" is present both as an optional and required
  dependency
  [1]
  $ cat > dune <<'EOF'
  > (library (name foo) (modules)
  >  (libraries missing (select choice.ml from (!missing -> choice.none.ml))))
  > EOF
  $ dune build
  File "dune", line 2, characters 1-73:
  2 |  (libraries missing (select choice.ml from (!missing -> choice.none.ml))))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: library "missing" is present both as a forbidden and required
  dependency
  [1]
  $ cat > dune <<'EOF'
  > (library (name foo) (modules)
  >  (libraries (select choice.ml from (!missing -> choice.none.ml)) missing))
  > EOF
  $ dune build
  File "dune", line 2, characters 1-73:
  2 |  (libraries (select choice.ml from (!missing -> choice.none.ml)) missing))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: library "missing" is present both as a forbidden and required
  dependency
  [1]

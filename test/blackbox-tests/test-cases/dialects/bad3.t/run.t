Rejects dialect extensions that start with a dot.

  $ dune build
  File "dune-project", line 5, characters 28-32:
  5 |  (implementation (extension .foo))
                                  ^^^^
  Error: extension must not start with '.'
  [1]

An extension cannot contain a directory separator either.

  $ rm dune-project
  $ cat > dune-project <<'EOF'
  > (lang dune 3.9)
  > (dialect
  >  (name custom)
  >  (implementation (extension foo/bar)))
  > EOF
  $ dune build
  File "dune-project", line 4, characters 28-35:
  4 |  (implementation (extension foo/bar)))
                                  ^^^^^^^
  Error: extension must start with '.' and not contain '/'.
  [1]

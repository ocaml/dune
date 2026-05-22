Writing `(package ...)` inside a named dependency binding like
`(:name (package foo))` is rejected: the binding would resolve to an
empty path list, which is rarely what the user intended.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<EOF
  > (library (public_name mypkg))
  > EOF
  $ cat >src/mypkg.ml <<'EOF'
  > let x = 1
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (:pkg (package mypkg)))
  >  (action (with-stdout-to out (echo %{pkg}))))
  > EOF

  $ dune build out 2>&1
  File "dune", line 2, characters 22-27:
  2 |  (deps (:pkg (package mypkg)))
                            ^^^^^
  Error: (package ...) is not supported inside a named dependency binding
  (:pkg).
  Hint: Place the (package ...) entry in the deps list directly.
  [1]

Putting the package outside the named binding works:

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mypkg))
  >  (action (with-stdout-to out (echo done))))
  > EOF
  $ dune build out

Writing `(package ...)` inside a named dependency binding like
`(:name (package foo))` is rejected: the binding would resolve to an
empty path list, which is rarely what the user intended.

  $ make_mypkg_lib_project

The restriction is currently not checked when the rule is disabled:

  $ cat >dune <<'EOF'
  > (rule
  >  (target out)
  >  (enabled_if false)
  >  (deps (:pkg (package mypkg)))
  >  (action (echo unused)))
  > EOF
  $ dune build

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

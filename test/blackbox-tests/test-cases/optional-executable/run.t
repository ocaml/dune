Test optional executable
========================

  $ cat >dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries does-not-exist)
  >  (optional))
  > 
  > (rule
  >  (alias run-x)
  >  (action (run %{exe:x.exe})))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (package (name x))
  > EOF

  $ touch x.ml

  $ dune build @install

  $ dune build @all
  File "dune", line 3, characters 12-26:
  3 |  (libraries does-not-exist)
                  ^^^^^^^^^^^^^^
  Error: Library "does-not-exist" not found.
  Hint: try: dune external-lib-deps --missing @all
  [1]

  $ dune build @run-x
  File "dune", line 3, characters 12-26:
  3 |  (libraries does-not-exist)
                  ^^^^^^^^^^^^^^
  Error: Library "does-not-exist" not found.
  Hint: try: dune external-lib-deps --missing @run-x
  [1]

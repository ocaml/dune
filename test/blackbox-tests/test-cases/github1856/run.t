Optional implementation of virtual library
  $ dune build @install
  File "impl/dune", line 2, characters 7-15:
  2 |  (name foo_impl)
             ^^^^^^^^
  Error: Library "foo.impl" in _build/default/impl is hidden (optional with unavailable dependencies).
  Hint: try: dune external-lib-deps --missing @install
  [1]

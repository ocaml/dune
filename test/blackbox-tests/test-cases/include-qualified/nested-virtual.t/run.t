We can nested modules virtual
  $ dune build @all
  File "vlib/dune", line 5, characters 18-26:
  5 |  (virtual_modules bar/virt))
                        ^^^^^^^^
  Error: Module Bar/virt doesn't exist.
  [1]

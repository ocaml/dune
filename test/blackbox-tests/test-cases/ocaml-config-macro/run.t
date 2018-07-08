%{ocaml-config:...} macros should be available. we don't print anything because
the values are all platform specific.

  $ dune build
  File "dune", line 3, characters 41-61:
  Error: Unknown form: %{ocaml-config:system}
  [1]
This one uses forbidden variables
  $ dune build foo
  File "dune", line 3, characters 16-31:
  3 |  (enabled_if (= %{project_root} "")))
                      ^^^^^^^^^^^^^^^
  Error: Only context_name, profile, architecture, system, model, os_type,
  ccomp_type and ocaml_version variables are allowed in this 'enabled_if'
  field. Please upgrade your dune language to at least 3.15.
  [1]

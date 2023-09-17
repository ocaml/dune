This one uses forbidden variables
  $ dune build foo
  File "dune", line 3, characters 16-31:
  3 |  (enabled_if (= %{project_root} "")))
                      ^^^^^^^^^^^^^^^
  Error: Only context_name, profile, architecture, system, model, os_type,
  ccomp_type and ocaml_version variables are allowed in this 'enabled_if'
  field. If you think that project_root should also be allowed, please file an
  issue about it.
  [1]

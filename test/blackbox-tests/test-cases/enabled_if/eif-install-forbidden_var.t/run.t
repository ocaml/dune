Tests for enabled_if in install stanza using forbidden variable.
  $ dune build @install
  File "dune", line 6, characters 16-31:
  6 |  (enabled_if (= %{project_root} ""))
                      ^^^^^^^^^^^^^^^
  Error: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version, context_name and arch_sixtyfour variables are allowed in this
  'enabled_if' field. Please upgrade your dune language to at least 3.15.
  [1]

Tests for enabled_if in install stanza using forbidden variable.
  $ make_dune_project 2.6
  $ dune build @install
  File "dune", line 6, characters 17-32:
  6 |  (enabled_if (<> %{project_root} ""))
                       ^^^^^^^^^^^^^^^
  Error: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version, context_name, arch_sixtyfour and env variables are allowed in
  this 'enabled_if' field. Please upgrade your dune language to at least 3.15.
  [1]

For dune >= 3.25, any vars are allowed in the install stanza
  $ make_dune_project 3.25
  $ dune build @install
  $ ls _build/install/default/bin/
  foo.x

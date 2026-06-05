The next ones use forbidden variables For dune 2.3 -> 2.5 it is a warning

  $ make_dune_project 2.3
  $ dune exec ./foo.exe
  File "dune", line 3, characters 17-32:
  3 |  (enabled_if (<> %{project_root} "")))
                       ^^^^^^^^^^^^^^^
  Warning: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version, context_name, arch_sixtyfour and env variables are allowed in
  this 'enabled_if' field. Please upgrade your dune language to at least 3.15.
  bar

For dune >= 2.6 it is an error
  $ make_dune_project 2.6
  $ dune exec ./foo.exe
  File "dune", line 3, characters 17-32:
  3 |  (enabled_if (<> %{project_root} "")))
                       ^^^^^^^^^^^^^^^
  Error: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version, context_name, arch_sixtyfour and env variables are allowed in
  this 'enabled_if' field. Please upgrade your dune language to at least 3.15.
  [1]

In dune >= 3.25, we allow use of any variables in the executable stanza
  $ make_dune_project 3.25
  $ dune exec ./foo.exe
  bar

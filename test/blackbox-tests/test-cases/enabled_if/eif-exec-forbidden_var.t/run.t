The next ones use forbidden variables For dune 2.3 -> 2.5 it is a warning

  $ cat > dune-project <<EOF
  > (lang dune 2.3)
  > EOF
  $ dune exec ./foo.exe
  File "dune", line 3, characters 17-32:
  3 |  (enabled_if (<> %{project_root} "")))
                       ^^^^^^^^^^^^^^^
  Warning: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version, context_name and arch_sixtyfour variables are allowed in this
  'enabled_if' field. Please upgrade your dune language to at least 3.15.
  bar

For dune >= 2.6 it is an error
  $ cat > dune-project <<EOF
  > (lang dune 2.6)
  > EOF
  $ dune exec ./foo.exe
  File "dune", line 3, characters 17-32:
  3 |  (enabled_if (<> %{project_root} "")))
                       ^^^^^^^^^^^^^^^
  Error: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version, context_name and arch_sixtyfour variables are allowed in this
  'enabled_if' field. Please upgrade your dune language to at least 3.15.
  [1]

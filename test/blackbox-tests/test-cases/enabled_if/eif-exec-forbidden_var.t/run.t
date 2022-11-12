The next ones use forbidden variables For dune 2.3 -> 2.5 it is a warning

  $ cat > dune-project <<EOF
  > (lang dune 2.3)
  > EOF
  $ dune exec ./foo.exe
  File "dune", line 3, characters 17-32:
  3 |  (enabled_if (<> %{project_root} "")))
                       ^^^^^^^^^^^^^^^
  Warning: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version and context_name variables are allowed in this 'enabled_if'
  field. If you think that project_root should also be allowed, please file an
  issue about it.
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
  ocaml_version and context_name variables are allowed in this 'enabled_if'
  field. If you think that project_root should also be allowed, please file an
  issue about it.
  [1]

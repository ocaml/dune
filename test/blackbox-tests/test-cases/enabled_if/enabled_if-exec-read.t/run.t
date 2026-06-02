Test that `enabled_if` fields work as expected for executables with read pform.

Check that enabled_if doesn't work before dune lang 2.3

  $ make_dune_project 2.0
  $ dune exec ./main.exe
  File "dune", line 5, characters 1-59:
  5 |  (enabled_if (= %{read:./config/enable_executable} "true")))
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'enabled_if' is only available since version 2.3 of the dune language.
  Please update your dune-project file to have (lang dune 2.3).
  [1]

Check that only common vars are allowed in enabled_if within the executable
stanza in older dune lang versions:

  $ make_dune_project 3.0
  $ dune exec ./main.exe
  File "dune", line 5, characters 16-50:
  5 |  (enabled_if (= %{read:./config/enable_executable} "true")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version, context_name, arch_sixtyfour and env variables are allowed in
  this 'enabled_if' field. Please upgrade your dune language to at least 3.15.
  [1]

Check that the executable is correctly enabled:

  $ make_dune_project 3.24
  $ dune exec ./main.exe
  File "dune", line 5, characters 16-50:
  5 |  (enabled_if (= %{read:./config/enable_executable} "true")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version, context_name, arch_sixtyfour and env variables are allowed in
  this 'enabled_if' field. Please upgrade your dune language to at least 3.15.
  [1]

Check that the executable is correctly disabled:

  $ dune exec ./disabled.exe
  File "dune", line 5, characters 16-50:
  5 |  (enabled_if (= %{read:./config/enable_executable} "true")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version, context_name, arch_sixtyfour and env variables are allowed in
  this 'enabled_if' field. Please upgrade your dune language to at least 3.15.
  [1]

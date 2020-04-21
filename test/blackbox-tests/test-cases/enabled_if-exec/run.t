Test that `enabled_if` fields work as expected for executables.
Since 2.3.

This executable is disabled, any attempt to build it should fail:
  $ dune build dis.exe
  Error: Don't know how to build dis.exe
  [1]
  $ dune exec ./dis.exe
  Error: Program "./dis.exe" not found!
  [1]

This one is enabled
  $ dune exec ./main.exe
  Pong

Installing should silently ignore disabled executables
  $ dune build @install

This one unse forbidden variables
  $ dune exec ./foo.exe --root forbidden_var
  Entering directory 'forbidden_var'
  File "dune", line 3, characters 19-32:
  3 |   (enabled_if (= %{project_root} "")))
                         ^^^^^^^^^^^^^
  Error: Only architecture, system, model, os_type, ccomp_type, profile and
  ocaml_version are allowed in this 'enabled_if' field.
  [1]

Tests for enabled_if in install stanza. Only bar.x should be installed.
  $ dune build @install --root install
  Entering directory 'install'
  $ ls install/_build/install/default/bin
  bar.x

Tests for enabled_if in install stanza using forbidden variable.
  $ dune build @install --root install/forbidden_var
  Entering directory 'install/forbidden_var'
  File "dune", line 6, characters 18-31:
  6 |  (enabled_if (= %{project_root} ""))
                        ^^^^^^^^^^^^^
  Error: Only architecture, system, model, os_type, ccomp_type, profile and
  ocaml_version are allowed in this 'enabled_if' field.
  [1]

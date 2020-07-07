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
  Error: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version and context_name variables are allowed in this 'enabled_if'
  field. If you think that project_root should also be allowed, please file an
  issue about it.
  [1]

The next ones use forbidden variables
For dune 2.3 -> 2.5 it is a warning
  $ cat > forbidden_var/dune-project <<EOF
  > (lang dune 2.3)
  > EOF
  $ dune exec ./foo.exe --root forbidden_var
  Entering directory 'forbidden_var'
  File "dune", line 3, characters 19-32:
  3 |  (enabled_if (<> %{project_root} "")))
                         ^^^^^^^^^^^^^
  Warning: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version and context_name variables are allowed in this 'enabled_if'
  field. If you think that project_root should also be allowed, please file an
  issue about it.
  Entering directory 'forbidden_var'
  bar

For dune >= 2.6 it is an error
  $ cat > forbidden_var/dune-project <<EOF
  > (lang dune 2.6)
  > EOF
  $ dune exec ./foo.exe --root forbidden_var
  Entering directory 'forbidden_var'
  File "dune", line 3, characters 19-32:
  3 |  (enabled_if (<> %{project_root} "")))
                         ^^^^^^^^^^^^^
  Error: Only architecture, system, model, os_type, ccomp_type, profile,
  ocaml_version and context_name variables are allowed in this 'enabled_if'
  field. If you think that project_root should also be allowed, please file an
  issue about it.
  [1]


For dune < 2.7 context_name is not allowed
  $ cat > var_context_name/dune-project <<EOF
  > (lang dune 2.6)
  > EOF
  $ dune exec ./foo.exe --root var_context_name
  Entering directory 'var_context_name'
  File "dune", line 3, characters 18-31:
  3 |  (enabled_if (= %{context_name} "default")))
                        ^^^^^^^^^^^^^
  Error: This variable is only available since version 2.7 of the dune
  language. Please update your dune-project file to have (lang dune 2.7).
  [1]

For dune >= 2.7 context_name allowed
  $ cat > var_context_name/dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ dune exec ./foo.exe --root var_context_name
  Entering directory 'var_context_name'
  Entering directory 'var_context_name'
  bar

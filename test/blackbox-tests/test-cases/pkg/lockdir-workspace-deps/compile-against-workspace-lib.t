A locked opaque recipe must receive workspace package variables and installed
artifacts through the same materialiser as other recipe dependencies.

  $ make_workspace_lib_package

The recipe checks metadata and section paths, compiles against the workspace
library, and records the result. Merely adding an OCAMLPATH entry is not enough:
the library files must be materialised and tracked as dependencies.

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends workspace-lib)
  > (build
  >  (system "set -eu; test '%{pkg:workspace-lib:name}' = workspace-lib; test '%{pkg:workspace-lib:version}' = dev; test '%{pkg:workspace-lib:installed}' = true; test -f '%{pkg:workspace-lib:lib}/workspace_lib.cma'; echo 'let () = print_endline Workspace_lib.greeting' > main.ml; ocamlc -I '%{pkg:workspace-lib:lib}' workspace_lib.cma main.ml -o main.exe; ./main.exe > result"))
  > (install
  >  (system "mkdir -p %{share}; cp result %{share}/result"))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

Lock-dir validation does not currently recognise workspace packages as
valid dependency targets, so the consumer's build action never runs:

  $ dune build out 2>&1
  File "_build/_private/default/.lock/dune.lock/consumer.pkg", line 2,
  characters 9-22:
  The package "consumer" depends on the package "workspace-lib", but
  "workspace-lib" does not appear in the lockdir
  _build/_private/default/.lock/dune.lock.
  Error: At least one package dependency is itself not present as a package in
  the lockdir _build/_private/default/.lock/dune.lock.
  Hint: This could indicate that the lockdir is corrupted. Delete it and then
  regenerate it by running: 'dune pkg lock'
  [1]

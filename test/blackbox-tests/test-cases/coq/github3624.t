This used to be a reproduction case for #3624, where dune created a
dune-project with an incorrect using line. Since we dropped support
for automatically creating the dune-project file, this is now testing
that the error message is good when the coq extension is not enabled.

  $ cat >dune <<EOF
  > (coq.theory
  >  (name foo))
  > EOF
  $ dune build
  Warning: No dune-project file has been found. A default one is assumed but
  the project might break when dune is upgraded. Please create a dune-project
  file.
  Hint: generate the project file with: $ dune init project <name>
  File "dune", line 1, characters 0-24:
  1 | (coq.theory
  2 |  (name foo))
  Error: 'coq.theory' is available only when coq is enabled in the dune-project
  file. You must enable it using (using coq 0.5) in your dune-project file.
  [1]

The test ensures we are able to run OxCaml tests for Dune.

  $ cat > dune-project << EOF
  > (lang dune 3.20)
  > EOF

  $ cat > main.ml << EOF
  > let i @ local = 42
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name main))
  > EOF

The test fails with an OxCaml error to demonstrate it compiled the ml file with
the correct compiler.
  $ dune build ./main.exe
  File "main.ml", line 1, characters 4-5:
  1 | let i @ local = 42
          ^
  Error: This value is local, but expected to be global because it is inside a module.
  [1]

Demonstrate what happens when the extension isn't enabled:

  $ cat >dune<<EOF
  > (rule
  >  (action (write-file foo bar))
  >  (targets foo)
  >  (enabled_if %{oxcaml_supported}))
  > EOF

  $ dune build ./foo
  File "dune", line 4, characters 13-32:
  4 |  (enabled_if %{oxcaml_supported}))
                   ^^^^^^^^^^^^^^^^^^^
  Error: Can't parse the variable oxcaml_supported without the oxcaml extension
  Hint: Try enabling the extension with (using oxcaml <version>)
  [1]

Adding the extension to the dune-project removes the error

  $ cat >> dune-project << EOF
  > (using oxcaml 0.1)
  > EOF

  $ dune build ./foo

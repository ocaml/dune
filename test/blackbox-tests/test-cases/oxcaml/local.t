The test ensures we are able to run OxCaml tests for Dune.

  $ cat > dune-project << EOF
  > (lang dune 3.18)
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

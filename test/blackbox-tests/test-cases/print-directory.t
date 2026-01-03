Test that "Entering directory" messages are only shown when dune changes
directory and there is actual output. Silent builds should not print anything.

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ cat > dune <<EOF
  > (library (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let x = 1
  > EOF

  $ mkdir subdir

Build the project first so subsequent builds have no output:
  $ dune build --root .

Running dune from a subdirectory with no output does not print directory messages:
  $ (cd subdir && dune build --root ..)

When there is output, the directory messages are shown:
  $ (cd subdir && dune build --root .. @nonexistent)
  Entering directory '..'
  Error: Alias "nonexistent" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  Leaving directory '..'
  [1]

The message can be suppressed with --no-print-directory even when there is output:
  $ (cd subdir && dune build --root .. --no-print-directory @nonexistent)
  Error: Alias "nonexistent" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]

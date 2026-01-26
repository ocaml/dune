Test that "Entering directory" messages are shown when dune changes directory,
even if there is no other output.

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

Running dune from a subdirectory shows the "Entering directory" message even
when there is no other output:
  $ (cd subdir && dune build --root ..)
  Entering directory '..'
  Leaving directory '..'

The message can be suppressed with --no-print-directory:
  $ (cd subdir && dune build --root .. --no-print-directory)

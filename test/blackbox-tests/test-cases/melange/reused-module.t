Test error message for modules belonging to melange.emit and another stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name lib)
  >  (modes melange))
  > (melange.emit
  >  (target output)
  >  (module_system commonjs))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build output/main.js
  File "dune", line 1, characters 0-0:
  Error: Module "Main" is used in several stanzas:
  - dune:1
  - dune:4
  To fix this error, you must specify an explicit "modules" field in every
  library, executable, and executables stanzas in this dune file. Note that
  each module cannot appear in more than one "modules" field - it must belong
  to a single library or executable.
  [1]

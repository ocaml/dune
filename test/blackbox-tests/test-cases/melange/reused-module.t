Test error message for modules belonging to melange.emit and another stanza

  $ make_melange_project 3.8 0.1

  $ cat > dune <<EOF
  > (library
  >  (name lib)
  >  (modes melange))
  > (melange.emit
  >  (emit_stdlib false)
  >  (target output))
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
  To fix this error, you must specify explicit "modules" fields so that each
  module belongs to only one stanza. Stanzas without an explicit "modules"
  field use all modules in the directory by default. This applies to library,
  executable, executables, test, tests, and melange.emit stanzas.
  [1]

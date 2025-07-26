Reproduction case for https://github.com/ocaml/dune/issues/12057

A mismatch between an ml file and its mli file usually gives an error message
mentioning both files. However in dune, it appears that we trigger the mention
of the .ml twice.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let x = 42 ;;
  > EOF

  $ cat > foo.mli <<EOF
  > val x : unit
  > EOF

  $ dune build
  File "foo.ml", line 1:
  Error: The implementation foo.ml does not match the interface foo.mli: 
         Values do not match: val x : int is not included in val x : unit
         The type int is not compatible with the type unit
         File "foo.mli", line 1, characters 0-12: Expected declaration
         File "foo.ml", line 1, characters 4-5: Actual declaration
  [1]

Compare this with a manual invocation where the error message displays
correctly.

  $ ocamlc foo.mli
  $ ocamlc foo.ml
  File "foo.ml", line 1:
  Error: The implementation foo.ml does not match the interface foo.mli: 
         Values do not match: val x : int is not included in val x : unit
         The type int is not compatible with the type unit
         File "foo.mli", line 1, characters 0-12: Expected declaration
         File "foo.ml", line 1, characters 4-5: Actual declaration
  [2]

The situation in dune is due to -intf-suffix .ml being passed:

  $ ocamlc foo.ml -intf-suffix .ml
  File "foo.ml", line 1:
  Error: The implementation foo.ml does not match the interface foo.ml: 
         Values do not match: val x : int is not included in val x : unit
         The type int is not compatible with the type unit
         File "foo.mli", line 1, characters 0-12: Expected declaration
         File "foo.ml", line 1, characters 4-5: Actual declaration
  [2]


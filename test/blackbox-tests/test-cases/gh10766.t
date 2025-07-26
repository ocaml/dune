In OCaml 5.0, the automatic inclusion of the unix library generates a warning.
The compiler provides a none location since it does not know who was
responsible for providing the linking flags. In dune, we can try to do a bit
better and provide the location of the dune file.

  $ cat > dune-project <<EOF
  > (lang dune 3.20)
  > EOF

  $ cat > foo.ml <<EOF
  > let x = Unix.realdir ;;
  > EOF

  $ cat > dune <<EOF
  > (executable (name foo))
  > EOF

  $ dune build
  File "dune", line 1, characters 0-23:
  1 | (executable (name foo))
      ^^^^^^^^^^^^^^^^^^^^^^^
  File "_none_", line 1:
  Alert ocaml_deprecated_auto_include: 
  OCaml's lib directory layout changed in 5.0. The unix subdirectory has been
  automatically added to the search path, but you should add -I +unix to the
  command-line to silence this alert (e.g. by adding unix to the list of
  libraries in your dune file, or adding use_unix to your _tags file for
  ocamlbuild, or using -package unix for ocamlfind).
  
  File "foo.ml", line 1, characters 8-20:
  1 | let x = Unix.realdir ;;
              ^^^^^^^^^^^^
  Error: Unbound value Unix.realdir
  Hint: Did you mean readdir?
  [1]


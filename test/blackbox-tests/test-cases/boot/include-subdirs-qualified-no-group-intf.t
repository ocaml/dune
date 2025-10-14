Testing the bootstrap of a wrapped include subdirs qualified.

Currently doesn't work because it is not implemented.

  $ . ./helpers.sh

  $ mkdir -p src/a/b

  $ cat > src/a/b/x.ml <<EOF
  > let () = print_endline "Hello from wrapped a/b/x.ml"
  > EOF

  $ cat > src/a/dune <<EOF
  > (library (name a))
  > (include_subdirs qualified)
  > EOF

  $ create_dune a <<EOF
  > module M1 = A.B
  > module M2 = A.B.X
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  Hello from wrapped a/b/x.ml
  Hello from bootstrapped binary!


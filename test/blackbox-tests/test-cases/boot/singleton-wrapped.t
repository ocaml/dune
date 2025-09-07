Testing the bootstrap of singleton wrapped libraries.

  $ . ./helpers.sh

  $ mkdir -p src/a

  $ cat > src/a/a.ml <<EOF
  > let () = Printf.printf "Hello from singleton wrapped a/a.ml\n"
  > EOF

  $ cat > src/a/dune <<EOF
  > (library
  >  (name a))
  > EOF

  $ create_dune a <<EOF
  > open A
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  Hello from singleton wrapped a/a.ml
  Hello from bootstrapped binary!


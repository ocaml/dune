Testing the bootstrap of singleton unwrapped libraries.

  $ . ./helpers.sh

  $ mkdir -p src/a

  $ cat > src/a/b.ml <<EOF
  > let () = Printf.printf "Hello from singleton unwrapped a/b.ml\n"
  > EOF

  $ cat > src/a/dune <<EOF
  > (library
  >  (name a)
  >  (wrapped false))
  > EOF

  $ create_dune a <<EOF
  > open B
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  Hello from singleton unwrapped a/b.ml
  Hello from bootstrapped binary!


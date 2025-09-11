Testing the bootstrap of wrapped libraries.

  $ . ./helpers.sh

  $ mkdir -p src/a

  $ cat > src/a/a.ml <<EOF
  > module B = B
  > let () = Printf.printf "Hello from wrapped interface module a/a.ml\n"
  > EOF

  $ cat > src/a/b.ml <<EOF
  > let () = Printf.printf "Hello from wrapped module a/b.ml\n"
  > EOF

  $ cat > src/a/dune <<EOF
  > (library
  >  (name a))
  > EOF

  $ create_dune a <<EOF
  > open A
  > open B
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  Hello from wrapped module a/b.ml
  Hello from wrapped interface module a/a.ml
  Hello from bootstrapped binary!


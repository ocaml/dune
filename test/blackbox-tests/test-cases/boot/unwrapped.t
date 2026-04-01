Testing the bootstrap of unwrapped libraries.

  $ init_bootstrap

  $ mkdir -p src/a
  $ cat > src/a/a.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/a.ml\n"
  > EOF

  $ cat > src/a/b.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/b.ml\n"
  > EOF

  $ cat > src/a/dune <<EOF
  > (library
  >  (name a)
  >  (wrapped false))
  > EOF

  $ make_module src/a/root.ml

  $ create_dune a <<EOF
  > open Root
  > open A
  > open B
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  Hello from unwrapped a/a.ml
  Hello from unwrapped a/b.ml
  Hello from bootstrapped binary!


Testing the bootstrap of a wrapped include subdirs qualified.

Currently doesn't work because it is not implemented.

  $ . ./helpers.sh

  $ mkdir -p src/a/b/c

  $ cat > src/a/x.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/x.ml\n"
  > EOF

  $ cat > src/a/b/b.ml <<EOF
  > let () = Printf.printf "Hello form wrapped a/b/b.ml\n"
  > EOF

  $ cat > src/a/b/c/c.ml <<EOF
  > let () = Printf.printf "Hello form wrapped a/b/c/c.ml\n"
  > EOF

  $ cat > src/a/dune <<EOF
  > (library
  >  (name a))
  > (include_subdirs qualified)
  > EOF

  $ create_dune a <<EOF
  > open A
  > open X
  > open B
  > open C
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  Fatal error: exception Failure("failed to find [B;A]")
  [2]


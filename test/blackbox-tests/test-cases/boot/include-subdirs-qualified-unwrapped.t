Testing the bootstrap of an unwrapped include subdirs qualified.

Currently doesn't work because it is not implemented.

  $ . ./helpers.sh

  $ mkdir -p src/lib/b/c

  $ cat > src/lib/x.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/x.ml\n"
  > EOF

  $ cat > src/lib/b/b.ml <<EOF
  > module C = C
  > let () = Printf.printf "Hello from unwrapped a/b/b.ml\n"
  > EOF

  $ cat > src/lib/b/c/c.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/b/c/c.ml\n"
  > EOF

  $ cat > src/lib/dune <<EOF
  > (library
  >  (name lib)
  >  (wrapped false))
  > (include_subdirs qualified)
  > EOF

  $ create_dune lib <<EOF
  > module M1 = X
  > module M2 = B
  > module M3 = B.C
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  Hello from unwrapped a/b/c/c.ml
  Hello from unwrapped a/b/b.ml
  Hello from unwrapped a/x.ml
  Hello from bootstrapped binary!


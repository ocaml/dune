Testing the bootstrap of an unwrapped include subdirs unqualified.

  $ . ./helpers.sh

  $ mkdir -p src/a/b/c

  $ cat > src/a/x.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/x.ml\n"
  > EOF

  $ cat > src/a/b/b.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/b/b.ml\n"
  > EOF

  $ cat > src/a/b/c/c.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/b/c/c.ml\n"
  > EOF

  $ cat > src/a/dune <<EOF
  > (library
  >  (name a)
  >  (wrapped false))
  > (include_subdirs unqualified)
  > EOF

  $ create_dune a <<EOF
  > open X
  > open B
  > open C
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  Hello from unwrapped a/b/b.ml
  Hello from unwrapped a/b/c/c.ml
  Hello from unwrapped a/x.ml
  Hello from bootstrapped binary!


Testing the bootstrap of an unwrapped include subdirs qualified.

Currently doesn't work because it is not implemented.

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
  > (include_subdirs qualified)
  > EOF

  $ create_dune a <<EOF
  > open X
  > open B
  > open C
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  cd _boot && /OCAMLOPT -c -g -no-alias-deps -w -49-23-53 -alert -unstable dune_exe__Main.ml
  File "dune_exe__Main.ml", line 3, characters 5-6:
  3 | open C
           ^
  Error: Unbound module C
  [2]


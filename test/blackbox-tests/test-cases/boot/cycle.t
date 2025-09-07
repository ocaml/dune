Testing cycle detection in bootstrap.

  $ . ./helpers.sh

  $ mkdir -p src/a
  $ cat > src/a/a.ml <<EOF
  > open B
  > EOF

  $ cat > src/a/b.ml <<EOF
  > open A
  > EOF

  $ cat > src/a/dune <<EOF
  > (library
  >  (name a))
  > EOF

  $ create_dune a <<EOF
  > open A
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  cycle:
  - a__B.ml
  - a.ml
  - dune_exe__Main.ml
  dependency cycle compiling a.ml
  [2]


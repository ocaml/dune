Testing cycle detection in bootstrap.

  $ init_bootstrap

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
  ocamllex -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  cycle:
  - a__B.ml
  - a.ml
  - dune_exe__Main.ml
  dependency cycle compiling a.ml
  [2]

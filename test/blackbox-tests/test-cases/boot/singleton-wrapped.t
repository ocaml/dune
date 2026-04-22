Testing the bootstrap of singleton wrapped libraries.

  $ init_bootstrap

  $ mkdir -p src/a

  $ make_module src/a/a.ml
  $ cat >> src/a/a.ml <<EOF
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
  ocamllex -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Hello from singleton wrapped a/a.ml
  Hello from bootstrapped binary!

Testing the bootstrap of singleton unwrapped libraries.

  $ init_bootstrap

  $ mkdir -p src/a

  $ make_module src/a/b.ml
  $ cat >> src/a/b.ml <<EOF
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
  ocamllex -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Hello from singleton unwrapped a/b.ml
  Hello from bootstrapped binary!

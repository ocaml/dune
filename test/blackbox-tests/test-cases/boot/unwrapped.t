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
  ocamllex -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Hello from unwrapped a/a.ml
  Hello from unwrapped a/b.ml
  Hello from bootstrapped binary!

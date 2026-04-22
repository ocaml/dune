Testing the bootstrap of wrapped libraries without interface moodule.

  $ init_bootstrap

  $ mkdir -p src/a

  $ cat > src/a/b.ml <<EOF
  > let () = Printf.printf "Hello from wrapped non-interface module a/b.ml\n"
  > EOF

  $ make_module src/a/root.ml

  $ cat > src/a/dune <<EOF
  > (library
  >  (name a))
  > EOF

  $ create_dune a <<EOF
  > open A
  > open B
  > open Root
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamllex -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Hello from wrapped non-interface module a/b.ml
  Hello from bootstrapped binary!
